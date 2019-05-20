#lang racket

(require
  )

(provide
  SEMMED
  RTX
  ROBOKOP
  ORANGE
  add-concept-key/cid-associations-to-hashtable!)

(define SEMMED 'SEMMED)
(define RTX 'RTX)
(define ROBOKOP 'ROBOKOP)
(define ORANGE 'ORANGE)

(define DB/concept-file-alist
  '((SEMMED . "data/semmed/concepts.scm")
    (RTX . "data/rtx/concepts.scm")
    (ROBOKOP . "data/robokop/concepts.scm")
    (ORANGE . "data/orange/concepts.scm")))

(define DB/props-key-alist
  '((SEMMED . "xrefs")
    (RTX . "id")
    (ROBOKOP . "equivalent_identifiers")
    (ORANGE . "same_as")))


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
      (let ((pr (assoc props-key props)))
        (and pr
             (let ((str (cdr pr)))
               (let ((keys (regexp-match* key-regex str #:match-select cadr)))
                 keys)))))))
