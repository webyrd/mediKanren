#lang racket

(require
  )

(provide
  SEMMED
  RTX
  ROBOKOP
  ORANGE
  HASHTABLE_SAVE_DIRECTORY
  filled-hashtable?
  add-concept-key/cid-associations-to-hashtable!
  save-hashtable!
  load-hashtable  
  load-or-create/save-hashtable!
  )

(define SEMMED 'semmed)
(define RTX 'rtx)
(define ROBOKOP 'robokop)
(define ORANGE 'orange)

(define HASHTABLE_SAVE_DIRECTORY "tmp")

(define DB/concept-file-alist
  '((semmed . "data/semmed/concepts.scm")
    (rtx . "data/rtx/concepts.scm")
    (robokop . "data/robokop/concepts.scm")
    (orange . "data/orange/concepts.scm")))

(define DB/props-key-alist
  '((semmed . "xrefs")
    (rtx . "id")
    (robokop . "equivalent_identifiers")
    (orange . "same_as")))


(define filled-hashtable?
  (lambda (ht)
    (and ht (hash? ht) (> (hash-count ht) 0))))

(define (add-concept-key/cid-associations-to-hashtable! ht)
  (lambda (db-name key-regex)
    (let ((concepts-file-name (cdr (assoc db-name DB/concept-file-alist)))
          (props-key (cdr (assoc db-name DB/props-key-alist))))
      (let ((concept-vector->keys (make-prop-lookup props-key key-regex)))
        (let ((ip (open-input-file concepts-file-name)))
          (let loop ([cid 0]
                     [data (read ip)])
            (cond
              [(eof-object? data)
               (close-input-port ip)
               (void)]
              [else
               (when (= (modulo cid 10000) 0)
                 (printf "~s cid: ~s\n" db-name cid))
               (let ((keys (concept-vector->keys data)))
                 (when keys
                   (for-each
                     (lambda (key)
                       (let ((entry (hash-ref ht key '())))
                         (hash-set! ht key (cons (cons db-name cid) entry))))
                     keys)))
               (loop (add1 cid) (read ip))])))))))

(define (make-prop-lookup props-key key-regex)
  (lambda (data)
    (let ((props (vector-ref data 3)))
      ;; all the KG's have an 'id' entry with the CUI for the concept, which we should also include
      (define (find-cids-by-prop-list-key prop-list-key)
        (cond
          ((assoc prop-list-key props)
           =>
           (lambda (pr)
             (let ((str (cdr pr)))
               (let ((keys (regexp-match* key-regex str #:match-select cadr)))
                 keys))))
          (else '())))
      (define id-keys (find-cids-by-prop-list-key "id"))
      (define equivalent-keys (find-cids-by-prop-list-key props-key))
      (set->list
        (set-union
          (list->set id-keys)
          (list->set equivalent-keys))))))

(define (save-hashtable! ht ht-file-name)
  (define op (open-output-file ht-file-name #:exists 'replace))
  (write ht op)
  (close-output-port op))

(define (load-hashtable ht-file-name)
  (if (file-exists? ht-file-name)
      (let ()
        (define ip (open-input-file ht-file-name))
        (let ((ht (read ip)))
          (close-input-port ip)
          ht))
      #f))

(define (load-or-create/save-hashtable! ht-name load-ht fill-ht! save-ht!)
  (printf "attempting to load hashtable for ~s\n" ht-name)
  (let ((ht (load-ht)))
    (if (filled-hashtable? ht)
        (begin
          (printf "loaded non-empty hashtable for ~s, with ~s entries\n" ht-name (hash-count ht))
          ht)
        (begin
          (printf "unable to load a non-empty hashtable for ~s\n" ht-name)
          (printf "trying to create a hashtable for ~s...\n" ht-name)
          (fill-ht!)
          (printf "filled a hashtable for ~s. Trying to save hashtable...\n" ht-name)
          (save-ht!)
          (printf "saved the hashtable for ~s. Trying to load saved hashtable...\n" ht-name)
          (let ((ht (load-ht)))
            (printf "load the saved hashtable for ~s.  Checking if non-empty valid hashtable...\n" ht-name)
            (if (filled-hashtable? ht)
                (begin
                  (printf "loaded hashtable for ~s is non-empty, with ~s entries\n" ht-name (hash-count ht))
                  ht)
                (error "Unable to load or create hashtable ~s" ht-name)))))))
