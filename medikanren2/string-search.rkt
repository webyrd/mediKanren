#lang racket
(provide
 find-ids-named
 find-concepts-named
 string-search-init-rel
 make-stsopt
 stsopt-t
 param-fd-input-binary

 test:read-name-corpus
 test:suffix:corpus2->index-suffixes
 (prefix-out test: build-string-index-via-codec-and-write)
 (prefix-out test: build-string-index-via-codec)
 (prefix-out test: ensure-name-index-built)
 test:verify-corpus-index
 )
(require racket/dict)
(require racket/vector)
(require "base.rkt")
(require "string-search-config.rkt")
(require "string-search-impl.rkt")
(require "string-search-impl2.rkt")

;;; *** routines for index building ***
(define (report-memory)
  (collect-garbage 'major)
  (printf "current-memory-use ~aMB\n" (exact->inexact (/ (current-memory-use) 1000000))))

(define (suffix:corpus2->index-suffixes hashcorpus)
  (for*/vector (((foffs s-searchable) (in-hash hashcorpus))
              (soffs (range (string-length s-searchable))))
    (suffix-key->bytes (cons foffs soffs))))

(define (suffix:corpus2->index hashcorpus)
  (define (suffix<? a b)    (suffix<?/corpus2 hashcorpus a b))
  (define suffixes (suffix:corpus2->index-suffixes hashcorpus))
  (report-memory)
  (printf "sorting suffixes\n")
  (vector-sort! suffixes suffix<?)
  (printf "sorting complete\n")
  suffixes)

(define (test:suffix:corpus2->index-suffixes veccorpus)
  (define hashcorpus (in-pairs->hash veccorpus))
  (suffix:corpus2->index-suffixes hashcorpus))

;;; in-name-corpus
;;;   Opens a sequence produced by decoding a primary index file.  Auto-closes upon encountering eof.
(define (in-name-corpus fname absdOut)
  (define fdIn (open-input-file (expand-user-path (build-path absdOut fname)) #:mode 'binary))
  (define (read-decode fdIn)
    (if (port-closed? fdIn)
        eof
        (if (eof-object? (peek-byte fdIn))
            (begin
              (close-input-port fdIn)
              eof)
            (let* ((foffs (file-position fdIn))
                   (v (decode fdIn schema-pri)))
              (match (name-ornull-from-pri v)
                (`(,name)
                 (let ((s-searchable (string/searchable name)))
                   (cons s-searchable foffs)))
                (_ (read-decode fdIn)))))))
  (in-port read-decode fdIn))

(define (in-pairs->hash in-pairs)
  (let ((h (make-hasheqv))) ; TODO perf?
    (for ((kv in-pairs))
      (match kv
        ((cons s-searchable foffs)
          (hash-set! h foffs s-searchable))))
    h))

(define (read-name-corpus fname absdOut)
  (let ((in-corpus (in-name-corpus fname absdOut)))
    (in-pairs->hash in-corpus)))

(define (test:read-name-corpus fname absdOut)
  (let ((in-corpus (in-name-corpus fname absdOut)))
    (for*/vector ((kv in-corpus))
      kv)))


;;; build-string-index-via-codec
;; Name+id corpus must live on disk so as to possess file offsets.
(define (build-string-index-via-codec fname absdOut)
  (let ((hashcorpus (read-name-corpus fname absdOut)))
    (report-memory)
    (printf "building name search index...\n")
    (values hashcorpus (suffix:corpus2->index hashcorpus))))

(define (build-string-index-via-codec-and-write fn-pri fn-index absdOut)
  (define-values (hashcorpus name-index) (build-string-index-via-codec fn-pri absdOut))
  (printf "indexed ~a suffixes, now writing...\n" (vector-length name-index))
  (call-with-atomic-output-file
    (expand-user-path (build-path absdOut fn-index))
    (lambda (fd-index adir-junk)
      (for* ((ni name-index))
          (write-bytes ni fd-index)
          ))))

(define (ensure-name-index-built absd-index fn-concept-name-index)
  (let* (
         (absf-index (path->string (simplify-path (build-path absd-index fn-concept-name-index)))))
    ; (printf "checking for index ~a\n" absf-index)
    (unless (file-exists? absf-index)
      (build-string-index-via-codec-and-write fn-cprop-primary fn-concept-name-index absd-index))))

(define (assert-name-index-built absd-index fn-concept-name-index)
  (let* (
         (absf-index (path->string (simplify-path (build-path absd-index fn-concept-name-index)))))
    ; (printf "checking for index ~a\n" absf-index)
    (unless (file-exists? absf-index)
      (error (format "Use of string search requires index preparation.  Call string-search-init-rel: ~a" absf-index)))))

(define (test:verify-corpus-index hashcorpus index)
  (define (<=? c d) (not (string<? d c)))
  (define (shorten s) (substring s 0 (min (string-length s) 20)))
  (for ((i (range 1 (vector-length index))))
    (let* (
        (skey-prev (bytes->suffix-key (vector-ref index (- i 1)) 0))
        (skey (bytes->suffix-key (vector-ref index i) 0))
        (ss-prev (substring (hash-ref hashcorpus (car skey-prev)) (cdr skey-prev)))
        (ss (substring (hash-ref hashcorpus (car skey)) (cdr skey))))
      (unless (<=? ss-prev ss)
        (error (format "index out of sequence: ~a should be < ~a\n" (shorten ss-prev) (shorten ss)))))
    ))

;;; find-ids-named
;; Consult the string index associated with rel to find the ids
;; in rel with a (name) string matching every substr in substrs,
;; according to options stsopt.  If the associated string index
;; has not been previously prepared, fail.
(define (find-ids-named rel substrs (stsopt stsopt-default))
  (unless (andmap string? substrs)
    (error "find-ids-named: substrs must be a list of strings"))
  (define absd-index (hash-ref (relation-definition-info rel) 'path))
  (assert-name-index-built absd-index fn-concept-name-index)
    (let* ((pris (db:~name*->concept*/options stsopt absd-index fn-cprop-primary fn-concept-name-index substrs)))
      (map uri-from-pri pris)))

;;; find-ids-named
;; Consult the string index associated with rel to find the concepts
;; in rel with a (name) string matching every substr in substrs,
;; according to options stsopt.  If the associated string index
;; has not been previously prepared, fail.
(define (find-concepts-named rel substrs (stsopt stsopt-default))
  (let ((uris (find-ids-named rel substrs stsopt)))
    (define-relation/table (found xxx)
      'source-stream (map list uris))
    (define-relation (found2 id subj object)
      (found id)                                     ;; logical AND, as in fresh
      (rel id subj object))
    found2))

;;; string-search-init-rel
;; Prepare a string search index, if it has not already been prepared.
;; If the the string search index has already been prepared, string-search-init-rel
;; will exit quickly.  If no string search index is prepared, calls
;; to find-ids-named and find-concepts-named will fail.
(define (string-search-init-rel rel)
  (let* ((absd-index (hash-ref (relation-definition-info rel) 'path))
         (absf-primary (path->string (simplify-path (build-path absd-index fn-cprop-primary)))))
    ;; We check for the file to exist so that individual db/foo.rkt files can call
    ;; string-search-init-rel, and so that common.rkt can require db/foo.rkt
    ;; without crashing, even when the data for db/foo.rkt is not installed.
    (when (file-exists? absf-primary)
      (ensure-name-index-built absd-index fn-concept-name-index)
  )))
