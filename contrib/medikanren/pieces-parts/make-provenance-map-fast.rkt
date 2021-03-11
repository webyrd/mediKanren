#lang racket

(require
  racket/runtime-path)

;; Creates a map of the provenance of the edges of a KG, using the "provided_by" edge attribute.
;; Fast, since we read directly from the raw data files.

;; *** Change this string to match the name of the KG you want to map! ***
;(define kg-name "rtx2_2020_09_16")
;(define kg-name "textminingprovider")
;(define kg-name "pr-owl")
;(define kg-name "co-occur")
;(define kg-name "orange")

(define-runtime-path path:root "..")
(define (path/root relative-path) (build-path path:root relative-path))
(define path:data                 (path/root "data"))
(define (path/data relative-path) (build-path path:data kg-name relative-path))

(define edges-file-path (path/data "edges.scm"))


(define edge-provenance-hash
  (time
   (with-input-from-file
       edges-file-path
     (lambda ()
       (let ((ht (make-hash)))
         (let loop ((i 0)
                    (x (read)))
           (when (= (modulo i 100000) 0)
             (printf "read \n~s edges so far...\n" i)
             ;(printf "ht: ~s\n" ht)
             )
           (cond
             ((eof-object? x) ht)
             (else
              (let ((edge-properties-alist (vector-ref x 3)))
                (let ((provided_by-entry (assoc "provided_by" edge-properties-alist)))
                  (let ((key (if provided_by-entry
                                 (cdr provided_by-entry)
                                 '__no-provided-by-info__)))
                    (let ((count (hash-ref ht key #f)))
                      (begin
                        (if count
                            (hash-set! ht key (add1 count))
                            (hash-set! ht key 1))
                        (loop (add1 i) (read))))))))))))
     #:mode 'text)))

(printf "created edge-provenance hash table with ~s entries\n" (hash-count edge-provenance-hash))
(printf "edge-provenance hash table: ~s\n" edge-provenance-hash)


(define sorted-provenance-counts
  (sort
   (map (lambda (key) (cons key (hash-ref edge-provenance-hash key)))
        (hash-keys edge-provenance-hash))
   (lambda (e1 e2) (> (cdr e1) (cdr e2)))))

#|
Generate TSV file with the provenance, and counts
|#
(with-output-to-file (string-append kg-name "-provenance_count" ".tsv")
  (lambda ()
    (printf "provenance\tedge count\n")
    (for-each
      (lambda (key/count)
        (let ((key (car key/count))
              (count (cdr key/count)))
          (printf "~a\t~a\n" key count)))
      sorted-provenance-counts))
  #:mode 'text
  #:exists 'replace)
