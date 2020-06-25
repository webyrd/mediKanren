#lang racket
(provide (all-defined-out))

#|
name-file.edge.scm
:ID :START :END

ex-row: 1 ENSEMBL:ENSG00000004059 ENSEMBL:ENSP00000000233

name-file.edge-props.scm
:ID propname value

ex-row: 1 edge_label biolink:has_gene_product
        1 provided_by blah
        1 

name-file.node.scm
:ID

name-file.node-props.scm
:ID propname value 

|#


(define directory-path
  "/Users/michaelpatton/Desktop/data 2/merged/")

(define edges-file
  "merged-kg_edges.tsv")


(define export-path
  (format "~a" (path->string (find-system-path 'desk-dir))))

(define edges-export-path
  (format "~acovid19.edge.tsv" export-path))

(define edges-export-file
  (open-output-file edges-export-path))

(define edge-props-export-path
  (format "~acovid19.edgeprops.tsv" export-path))

(define edge-props-export-file
  (open-output-file edge-props-export-path))

(define input-edges
  (open-input-file (format "~a/~a" directory-path edges-file)))

(define input-nodes
  (open-input-file (format "~a/~a" directory-path nodes-file)))

(let* ((header (read-line input-edges))
       (header (string-split header "\t")))
  (let loop ((i 0)
             (line-str (read-line input-edges)))
    (cond
      ((eof-object? line-str)
       (close-input-port input-edges)
       (close-output-port edges-export-file)
       (close-output-port edge-props-export-file))
      (else
       (let ((line (string-split line-str "\t")))
         (fprintf edges-export-file "~a\t~a\t~a\n" i (car line) (cadr line))
         (let loop-inner ((props (cddr line))
                          (headers (cddr header)))
           (when (not (null? props))
             (unless (string=? "" (car props))
               (fprintf edge-props-export-file "~a\t~a\t~a\n" i (car headers) (car props)))             
             (loop-inner (cdr props) (cdr headers)))))
       (loop
        (+ 1 i)
        (read-line input-edges))))))




 
