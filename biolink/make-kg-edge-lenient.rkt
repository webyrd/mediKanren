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
|#

(define directory-path
  "data/covid19/")
(define export-path directory-path)

(define nodes-import-path
  (format "~acovid19.node.tsv" export-path))
(define nodes-import-file
  (open-input-file nodes-import-path))
(read-line nodes-import-file) ;; header
(define nodes (set))
(let loop ((line (read-line nodes-import-file)))
  (unless (eof-object? line)
    (set! nodes (set-add nodes line))
    (loop (read-line nodes-import-file))))

(define edges-file
  "merged-kg_edges.tsv")
(define edges-export-path
  (format "~acovid19.edge.tsv" export-path))
(define edge-props-export-path
  (format "~acovid19.edgeprop.tsv" export-path))

(define edges-export-file
  (open-output-file edges-export-path))
(fprintf edges-export-file ":ID\t:START\t:END\n")
(define edge-props-export-file
  (open-output-file edge-props-export-path))
(fprintf edge-props-export-file ":ID\tpropname\tvalue\n")
(define input-edges
  (open-input-file (format "~a~a" directory-path edges-file)))
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
       (let* ((line (string-split line-str "\t"))
	      (start (car line))
	      (end (cadr line)))
	 (if (not (and (set-member? nodes start) (set-member? nodes end)))
	     (begin
	       (printf "skipping edge\n")
	       (loop i (read-line input-edges)))
	 (begin
	   (fprintf edges-export-file "~a\t~a\t~a\n" i start end)
	   (let loop-inner ((props (cddr line))
			    (headers (cddr header)))
	     (when (not (null? props))
	       (unless (string=? "" (car props))
		 (fprintf edge-props-export-file "~a\t~a\t~s\n" i (car headers) (car props)))
	       (loop-inner (cdr props) (cdr headers))))
	   (loop
	    (+ 1 i)
	    (read-line input-edges)))))))))
