#lang racket/base
(require
  "csv.rkt"
  (except-in racket/control set)
  racket/set)

;; This is the pre-normalization pass, which generates two files:

;; * PREDICATE.scm
;; * SEMTYPE.scm

;; Each of these contains all unique string values for the corresponding
;; field types, sorted.  These are mapped to numeric indices during
;; normalization.

(define argv (current-command-line-arguments))
(define argv-expected '#(IN_CSV OUT_DIR))

(when (not (= (vector-length argv-expected) (vector-length argv)))
  (error 'cmd-line-args (format "expected ~s; given ~s" argv-expected argv)))

(define csv-file-name (vector-ref argv 0))
(define out-dir (vector-ref argv 1))

(define predicate-file-name (build-path out-dir "PREDICATE.scm"))
(define semtype-file-name (build-path out-dir "SEMTYPE.scm"))

(define (print-ordered-unique out-predicate out-semtype)
  (define (yield record)
    (define predicate (list-ref record 3))
    (define subject-semtype (list-ref record 6))
    (define object-semtype (list-ref record 10))
    (shift k (cons (list predicate subject-semtype object-semtype) k)))
  (lambda (in)
    (read-line in 'any) ;; Skip header.
    (let loop ((count 1)
               (seen-predicate (set))
               (seen-semtype (set))
               (next (reset (and ((csv-records yield) in) #f))))
      (when (= 0 (remainder count 100000))
        (printf "processed ~s rows\n" count)
        (flush-output out-predicate)
        (flush-output out-semtype))
      (if next
        (let* ((data (car next))
               (predicate (list-ref data 0))
               (subject-semtype (list-ref data 1))
               (object-semtype (list-ref data 2))
               (k (cdr next))
               (seen-predicate2
                 (if (set-member? seen-predicate predicate)
                   seen-predicate
                   (begin
                     (printf "~s\n" `(new predicate: ,predicate))
                     (set-add seen-predicate predicate))))
               (seen-semtype2
                 (if (set-member? seen-semtype subject-semtype)
                   seen-semtype
                   (begin
                     (printf "~s\n" `(new semtype: ,subject-semtype))
                     (set-add seen-semtype subject-semtype))))
               (seen-semtype3
                 (if (set-member? seen-semtype2 object-semtype)
                   seen-semtype2
                   (begin
                     (printf "~s\n" `(new semtype: ,object-semtype))
                     (set-add seen-semtype2 object-semtype)))))
          (loop (+ 1 count) seen-predicate2 seen-semtype3 (k #t)))
        (begin
          (printf "writing files:\n~s\n~s\n"
                  (expand-user-path predicate-file-name)
                  (expand-user-path semtype-file-name))
          (for ((predicate (sort (set->list seen-predicate) string<?)))
               (fprintf out-predicate "~s\n" predicate))
          (for ((semtype (sort (set->list seen-semtype) string<?)))
               (fprintf out-semtype "~s\n" semtype)))))))

;; Print the distinct values of the indexed field.
(time (call-with-output-file
        (expand-user-path predicate-file-name)
        (lambda (out-predicate)
          (call-with-output-file
            (expand-user-path semtype-file-name)
            (lambda (out-semtype)
              (call-with-input-file
                (expand-user-path csv-file-name)
                (print-ordered-unique out-predicate out-semtype)))))))
