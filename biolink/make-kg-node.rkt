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

(define nodes-file
  "merged-kg_nodes.tsv")

(define export-path
  (format "~a" (path->string (find-system-path 'desk-dir))))

(define node-export-path
  (format "~acovid19.node.tsv" export-path))

(define nodes-export-file
  (open-output-file node-export-path))

(define node-props-export-path
  (format "~acovid19.nodeprops.tsv" export-path))

(define node-props-export-file
  (open-output-file node-props-export-path))

(define input-nodes
  (open-input-file (format "~a/~a" directory-path nodes-file)))

(let* ((header (read-line input-nodes))
       (header (string-split header "\t")))
  (let loop ((seen-nodes (set))
             (line-str (read-line input-nodes)))
    (cond
      ((eof-object? line-str)
       (close-input-port input-nodes)
       (close-output-port nodes-export-file)
       (close-output-port node-props-export-file))
      (else
       (let* ((line (string-split line-str "\t"))
              (node (car line)))
         (fprintf nodes-export-file "~a\n" node)
         (let loop-inner ((props (cdr line))
                          (headers (cdr header)))
           (when (not (null? props))
             (unless (string=? "" (car props))
               (fprintf node-props-export-file "~a\t~a\t~a\n" node (car headers) (car props)))             
             (loop-inner (cdr props) (cdr headers))))
         (loop        
          (set-add seen-nodes node)
          (read-line input-nodes)))))))




 
