#lang racket/base
(provide
  storage:filesystem
  storage-path
  storage-checkpoint-count
  storage-checkpoint-pending?
  storage-checkpoint!
  storage-revert!
  storage-trash-empty!
  storage-trash-empty?
  storage-description-keys
  storage-description-key?
  storage-description-ref
  storage-description-update!
  storage-description-set!
  storage-description-remove!
  storage-block-names
  storage-block-name?
  storage-block-path
  storage-block-new!
  storage-block-add-names!
  storage-block-remove-names!
  storage-block-rename!)
(require "logging.rkt" "misc.rkt"
         racket/file racket/pretty racket/set racket/struct)

;; This is a simple implementation of checkpointing storage.  It includes block storage for large
;; data, and key-value storage for small data and metadata.

;; Limitations:
;; - The default key-value store will not scale well to enormous amounts of data and/or enormous
;;   numbers of blocks (because these require tracking metadata).
;; - The default key-value store will not scale well to high frequency checkpoints.
;; - A single storage location should not be used by multiple host processes running concurrently.
;;   Initializing the same storage location from concurrent processes may lead to data corruption.
;; - Multiple threads of a single process can read concurrently from a single storage location,
;;   but it is not safe to read concurrently with writes or checkpoint commits to that location.
;; - It is not safe to perform concurrent writes or checkpoint commits to a single storage location.
;; - While it should be possible to recover from typical process failure and interruption, sudden
;;   host system failure and interruption may corrupt data.

;; Two forms of data can be stored:
;; - Description
;;   - "description" is a key-value map.
;;   - Keys and values can be any s-expression.
;;   - Keys can be added and removed.  A key's value can be updated.
;; - Block
;;   - Blocks are uninterpreted files of data.
;;   - Blocks are referenced by names.  A name can be any s-expression.  Each name maps to at most
;;     one block.  Multiple names can refer to the same block.
;;   - When a new block is created, it's data can be written and modified freely until the next
;;     checkpoint.  Once the checkpoint is committed, blocks cannot be updated.
;;   - Block data is only expected to be read after the checkpoint that creates them.
;;   - Blocks are not explicitly deleted.  A block is garbage collected once there are no longer
;;     any names referring to it after a checkpoint.

;; Storage system initialization and checkpoint commits will perform garbage collection of unused
;; blocks and old state descriptions.  By default, each collection will move garbage to a new
;; subdirectory under "trash", to make it possible to manually recover from mistakes.

;; A storage location is a self-contained directory with the following structure:
;;   block/
;;     filename-for-block-id
;;     ...
;;   metadata/
;;     current.scm
;;     [next.scm]
;;   trash/
;;     trash-uid/
;;       any-file ...
;;     ...
;;   [trash-pending/
;;     any-file ...]

;; Do not store other files anywhere within this directory structure.

;; The current state of the system is stored in metadata/current.scm.  While committing a
;; checkpoint, the next state temporarily lives in metadata/next.scm.  These states are Racket
;; s-expressions with the following shape:
;;   (hash
;;     'format-version  datum
;;     'checkpoint-time pretty-timestamp
;;     'data            (hash
;;                        'description (hash ,@{any-key any-value} ...)
;;                        'block       (hash ,@{any-name block-id} ...)))

(define (unique-directory path.root str.type)
    (define seconds (number->string (current-seconds)))
    (let loop ((id.local 0))
      (define candidate (string-append str.type "-" seconds "-" (number->string id.local)))
      (define apath     (path->string (build-path path.root candidate)))
      (cond ((directory-exists? apath) (loop (+ id.local 1)))
            (else                      apath))))

(define (storage-path                s) ((wrapped-storage-controller s) 'path))
(define (storage-checkpoint-count    s) ((wrapped-storage-controller s) 'checkpoint-count))
(define (storage-checkpoint-pending? s) ((wrapped-storage-controller s) 'checkpoint-pending?))
(define (storage-checkpoint!         s) ((wrapped-storage-controller s) 'checkpoint!))
(define (storage-revert!             s) ((wrapped-storage-controller s) 'revert!))
(define (storage-trash-empty!        s) ((wrapped-storage-controller s) 'trash-empty!))
(define (storage-trash-empty?        s) ((wrapped-storage-controller s) 'trash-empty?))

(define ((missing-key s key)) (error "storage description key does not exist" key (storage-path s)))

(define (storage-description-keys    s)                                          (hash-keys     ((wrapped-storage-controller s) 'description)))
(define (storage-description-key?    s key)                                      (hash-has-key? ((wrapped-storage-controller s) 'description) key))
(define (storage-description-ref     s key        (default (missing-key s key))) (hash-ref      ((wrapped-storage-controller s) 'description) key default))
(define (storage-description-update! s key update (default (missing-key s key))) (storage-description-set! s key (update (storage-description-ref s key default))))
(define (storage-description-set!    s . keys-values)                            ((wrapped-storage-controller s) 'description-set*!    keys-values))
(define (storage-description-remove! s . keys)                                   ((wrapped-storage-controller s) 'description-remove*! keys))

(define (storage-block-names         s)                      ((wrapped-storage-controller s) 'block-names))
(define (storage-block-name?         s name)                 ((wrapped-storage-controller s) 'block-name?         name))
(define (storage-block-path          s name)                 ((wrapped-storage-controller s) 'block-path          name))
(define (storage-block-new!          s         name . names) ((wrapped-storage-controller s) 'block-new!          (cons name names)))
(define (storage-block-add-names!    s name.current . names) ((wrapped-storage-controller s) 'block-add-names!    name.current names))
(define (storage-block-remove-names! s              . names) ((wrapped-storage-controller s) 'block-remove-names! names))
(define (storage-block-rename!       s name.old name.new)
  (storage-block-add-names!    s name.old name.new)
  (storage-block-remove-names! s name.old))

(define all-filesystem-storage-keys (weak-set))
(define (storage:filesystem path.storage)
  (let* ((path.storage (normalize-path path.storage))
         (key.storage  (list path.storage)))
    (unless (complete-path? path.storage)
      (error "storage path must be absolute" path.storage))
    (when (set-member? all-filesystem-storage-keys key.storage)
      (error "storage path already in use" path.storage))
    (set-add! all-filesystem-storage-keys key.storage)
    (wrapped-storage key.storage (make-filesystem-storage path.storage))))

(struct wrapped-storage (key controller)
        #:methods gen:custom-write
        ((define write-proc
           (make-constructor-style-printer
             (lambda (s) 'storage:filesystem)
             (lambda (s) (list (storage-path s)))))))

(define (make-state count data)
  (hash 'format-version   version.current
        'checkpoint-time  (pretty-timestamp)
        'checkpoint-count count
        'data             data))

(define dir.metadata        "metadata")
(define dir.block           "block")
(define dir.trash           "trash")
(define dir.trash-pending   "trash-pending")
(define fname.state.current "current.scm")
(define fname.state.next    "next.scm")
(define version.current     '2021-12-31)
(define data.empty          (hash 'description (hash)
                                  'block       (hash)))

(define (make-filesystem-storage path.storage)
  (define (local-path relpath)  (path->string (build-path path.storage relpath)))
  (define (desc)                (hash-ref current 'description))
  (define (block)               (hash-ref current 'block))
  (define (block-id? name)      (hash-ref (block) name #f))
  (define (block-id  name)      (or (block-id? name)
                                    (error "storage block name does not exist" name path.storage)))
  (define (id->path id)         (build-path path.block (number->string id)))
  (define (unique-block-id)     (+ (apply max
                                          (apply max 0 (set->list ids.new))
                                          (hash-values (hash-ref previous 'block)))
                                   1))
  (define (checkpoint-pending?) (not (and (set-empty? ids.new)
                                          (equal? current previous))))
  (define (collect-garbage!)
    (set-clear! ids.new)
    (define lpaths.all         (list->set (map path->string (directory-list path.block))))
    (define lpaths.reachable   (list->set (map number->string (hash-values (block)))))
    (define lpaths.unreachable (set->list (set-subtract lpaths.all lpaths.reachable)))
    (when (file-exists? path.state.next)
      (make-directory* path.trash-pending)
      (rename-file-or-directory path.state.next (build-path path.trash-pending fname.state.next)))
    (unless (null? lpaths.unreachable)
      (define numbers.unreachable  (map (lambda (lp) (or (string->number lp) lp)) lpaths.unreachable))
      (define unreachables.block   (filter number? numbers.unreachable))
      (define unreachables.unknown (filter string? numbers.unreachable))
      (unless (null? unreachables.unknown)
        (error "aborting garbage collection: found untracked files in block directory" unreachables.unknown path.storage))
      (pretty-log `(moving unreachable blocks to trash))
      (make-directory* path.trash-pending)
      (for-each (lambda (lp) (rename-file-or-directory (build-path path.block         lp)
                                                       (build-path path.trash-pending lp)))
                lpaths.unreachable))
    (when (directory-exists? path.trash-pending)
      (define upath (unique-directory path.trash "trash"))
      (rename-file-or-directory path.trash-pending upath)))

  (define path.metadata      (local-path dir.metadata))
  (define path.block         (local-path dir.block))
  (define path.trash         (local-path dir.trash))
  (define path.trash-pending (local-path dir.trash-pending))
  (define path.state.current (path->string (build-path path.metadata fname.state.current)))
  (define path.state.next    (path->string (build-path path.metadata fname.state.next)))
  (for-each make-directory* (list path.metadata path.block path.trash))
  (define-values (checkpoint-count previous)
    (let* ((state (cond ((file-exists? path.state.current) (call-with-input-file path.state.current read))
                        ((file-exists? path.state.next)    (pretty-log '(finishing transition for interrupted-yet-successful checkpoint commit))
                                                           (rename-file-or-directory path.state.next path.state.current)
                                                           (call-with-input-file path.state.current read))
                        (else                              (pretty-log '(creating new storage location) path.storage)
                                                           (define state.empty (make-state 0 data.empty))
                                                           (call-with-output-file path.state.current
                                                                                  (lambda (out) (pretty-write state.empty out)))
                                                           state.empty)))
           (version (hash-ref state 'format-version)))
      ;; TODO: attempt to migrate state to new format if version is old
      (unless (equal? version version.current)
        (error "storage state format version mismatch" `(found: ,version expected: ,version.current) path.storage))
      (values (hash-ref state 'checkpoint-count) (hash-ref state 'data))))
  (define current previous)
  (define ids.new (mutable-set))
  (pretty-log `(loaded storage state ,checkpoint-count for) path.storage)
  (collect-garbage!)
  (pretty-log `(collected garbage))

  (method-lambda
    ((path)                                path.storage)
    ((checkpoint-count)                    checkpoint-count)
    ((checkpoint-pending?)                 (checkpoint-pending?))
    ((description)                         (hash-ref current 'description))
    ((description-set*!    keys-values)    (set! current (hash-set current 'description
                                                                   (apply hash-set* (desc) keys-values))))
    ((description-remove*! keys)           (set! current (hash-set current 'description
                                                                   (foldl (lambda (key d) (hash-remove d key)) (desc) keys))))
    ((block-names)                         (hash-keys (block)))
    ((block-name?                    name) (hash-has-key? (block) name))
    ((block-path                     name) (id->path (block-id name)))
    ((block-new!                    names) (define b (block))
                                           (for-each (lambda (name)
                                                       (when (hash-has-key? b name)
                                                         (error "cannot create new block with existing name" name path.storage)))
                                                     names)
                                           (define id (unique-block-id))
                                           (set-add! ids.new id)
                                           (set! current (hash-set current 'block
                                                                   (foldl (lambda (name b) (hash-set b name id)) b names)))
                                           (id->path id))
    ((block-add-names! name.current names) (define b  (block))
                                           (define id (block-id name.current))
                                           (for-each (lambda (name)
                                                       (when (hash-has-key? b name)
                                                         (error "cannot add existing block name" name path.storage)))
                                                     names)
                                           (set! current (hash-set current 'block
                                                                   (foldl (lambda (name b) (hash-set b name id)) b names))))
    ((block-remove-names!           names) (set! current (hash-set current 'block
                                                                   (foldl (lambda (name b) (hash-remove b name)) (block) names))))

    ((checkpoint!)  (cond ((checkpoint-pending?)
                           (set! checkpoint-count (+ 1 checkpoint-count))
                           (pretty-log `(committing checkpoint ,checkpoint-count))
                           (define state (make-state checkpoint-count current))
                           (call-with-output-file path.state.next (lambda (out) (pretty-write state out)))
                           (make-directory* path.trash-pending)
                           (rename-file-or-directory path.state.current (build-path path.trash-pending fname.state.current))
                           (rename-file-or-directory path.state.next    path.state.current)
                           (set! previous current)
                           (pretty-log `(committed checkpoint ,checkpoint-count))
                           (collect-garbage!))
                          (else (pretty-log '(no checkpoint necessary: storage has not been modified)))))
    ((revert!)      (cond ((checkpoint-pending?)
                           (set! current previous)
                           (pretty-log `(reverted to checkpoint ,checkpoint-count))
                           (collect-garbage!))
                          (else (pretty-log '(no revert necessary: storage has not been modified)))))
    ((trash-empty?) (null? (directory-list path.trash #:build? #t)))
    ((trash-empty!) (define (delete-path path)
                      (cond ((file-exists?      path) (delete-file path))
                            ((directory-exists? path) (for-each delete-path (directory-list path #:build? #t))
                                                      (delete-directory path))))
                    (define dirs.trash (directory-list path.trash #:build? #t))
                    (cond ((null? dirs.trash) (pretty-log '(no trash to empty)))
                          (else               (pretty-log `(deleting ,(length dirs.trash) trash subdirectories))
                                              (for-each delete-path dirs.trash))))))
