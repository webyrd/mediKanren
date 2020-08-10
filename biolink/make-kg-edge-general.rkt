#lang racket
(provide (all-defined-out))

#|
name-file.edge.scm
:ID :START :END

ex-row: 1 ENSEMBL:ENSG00000004059 ENSEMBL:ENSP00000000233

name-file.edge-props.scm
:ID propname value
|#

(define directory-path
  ;; "data/textminingprovider/"
  "data/sri_semmeddb/"
  )
(define edges-file
  ;; "sample-craft-edges.v0.1.kgx.tsv"
  "semmeddb_edges.tsv"
  )
(define export-path directory-path)
(define edges-export-path
  (format
   ;; "~atextminingprovider.edge.tsv"
   "~asri_semmeddb.edge.tsv"
   export-path))
(define edge-props-export-path
  (format
   ;; "~atextminingprovider.edgeprop.tsv"
   "~asri_semmeddb.edgeprop.tsv"
   export-path))

(define edges-export-file
  (open-output-file edges-export-path))
(fprintf edges-export-file ":ID\t:START\t:END\n")
(define edge-props-export-file
  (open-output-file edge-props-export-path))
(fprintf edge-props-export-file ":ID\tpropname\tvalue\n")
(define input-edges
  (open-input-file (format "~a~a" directory-path edges-file)))
(let* ((header (read-line input-edges))
       (header (string-split header "\t" #:trim? #f)))
  (let loop ((i 0)
             (line-str (read-line input-edges)))
    (cond
      ((eof-object? line-str)
       (close-input-port input-edges)
       (close-output-port edges-export-file)
       (close-output-port edge-props-export-file))
      (else
        (let ((line (string-split line-str "\t" #:trim? #f)))
          (fprintf edges-export-file "~a\t~a\t~a\n" i (car line) (caddr line))
          (let loop-inner ((props (cons (cadr line) (cddr line)))
                           (headers (cons (cadr header) (cddr header))))
            (when (not (null? props))
              (unless (string=? "" (car props))
                (fprintf edge-props-export-file "~a\t~a\t~s\n" i (car headers) (car props)))
              (loop-inner (cdr props) (cdr headers)))))
        (loop
          (+ 1 i)
          (read-line input-edges))))))
