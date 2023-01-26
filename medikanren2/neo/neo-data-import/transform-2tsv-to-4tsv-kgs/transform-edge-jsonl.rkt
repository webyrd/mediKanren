#lang racket

(require json)
(provide transform-edge-jsonl)

#|
Output edge and edge-props file formats:
<name-file>.edge.scm
:ID :START :END
ex-row: 1 ENSEMBL:ENSG00000004059 ENSEMBL:ENSP00000000233
<name-file>.edge-props.scm
:ID propname value
ex-rows:
1	id	df6920caa6087ee642b8016b55776432
1	subject	GO:0061729
1	predicate	biolink:same_as
1	object	REACT:R-HSA-446205
|#

(define transform-edge-jsonl
  (lambda (edges-file-import-path
           edge-file-export-path
           edge-props-file-export-path)

    (printf "transform-edge-jsonl called\n")
    (printf "input edges jsonl: ~s\n" edges-file-import-path)
    (printf "output edge tsv: ~s\n" edge-file-export-path)
    (printf "output edge props tsv: ~s\n" edge-props-file-export-path)

    (define edges-export-out
      (open-output-file edge-file-export-path))
    (fprintf edges-export-out ":ID\t:START\t:END\n")
    (define edge-props-out
      (open-output-file edge-props-file-export-path))
    (fprintf edge-props-out ":ID\tpropname\tvalue\n")

    (define edges-in
      (open-input-file edges-file-import-path))

    (let loop ((id 0)
               (line-json (read-json edges-in)))
      (when (zero? (modulo id 100000))
        (printf "processing edges line ~s\n" id))
      (cond
        [(eof-object? line-json)
         (close-input-port edges-in)
         (close-output-port edges-export-out)
         (close-output-port edge-props-out)
         (printf "finished processing edges\n\n")]
        [else
         (let ((start (hash-ref line-json 'subject #f))
               (end (hash-ref line-json 'object #f)))
           (when (and start end)
             (fprintf edges-export-out "~a\t~a\t~a\n" id start end)
             (let loop-inner (;; all the properties, including the subject and onbject. The order might be various.
                              (propnames (hash-keys line-json)))
               (when (not (null? propnames))
                 (let* ((propname (car propnames))
                        (value (hash-ref line-json propname #f)))
                   (unless (equal? "" value)
                     (fprintf edge-props-out "~a\t~a\t~a\n" id propname value)))
                 (loop-inner (cdr propnames))))
	   ;; leave it when importing node_normalization KG, otherwise comment it out
             ;(fprintf edge-props-out "~a\tbiolink:primary_knowledge_source\tinfores:node-normalization\n" id)
             )
           (loop
            (add1 id)
            (read-json edges-in)))]))))
