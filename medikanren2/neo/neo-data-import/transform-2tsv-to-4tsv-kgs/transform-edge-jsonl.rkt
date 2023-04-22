#lang racket

(require json)
(provide transform-edge-jsonl)

#|
Output edge and edge-props file formats:
<name-file>.edge.tsv
:ID :START :END
ex-row: 1 ENSEMBL:ENSG00000004059 ENSEMBL:ENSP00000000233

<name-file>.edgeprops.tsv
:ID propname value
ex-rows:
1	id	df6920caa6087ee642b8016b55776432
1	subject	GO:0061729
1	predicate	biolink:same_as
1	object	REACT:R-HSA-446205

<name-file>.qualifiededge.tsv
:ID predicate object-aspect object-direction subject object
|#

(define transform-edge-jsonl
  (lambda (edges-file-import-path
           edge-file-export-path
           edge-props-file-export-path
           qualified-edge-file-export-path)

    (printf "transform-edge-jsonl called\n")
    (printf "input edges jsonl: ~s\n" edges-file-import-path)
    (printf "output edge tsv: ~s\n" edge-file-export-path)
    (printf "output edge props tsv: ~s\n" edge-props-file-export-path)
    (printf "output edge tsv: ~s\n" qualified-edge-file-export-path)

    (define edges-export-out
      (open-output-file edge-file-export-path))
    (fprintf edges-export-out ":ID\t:START\t:END\n")
    (define edge-props-out
      (open-output-file edge-props-file-export-path))
    (fprintf edge-props-out ":ID\tpropname\tvalue\n")
    (define qualified-edge-out
      (open-output-file qualified-edge-file-export-path))
    (fprintf qualified-edge-out ":ID\tpredicate\tobject-aspect\tobject-direction\tsubject\tobject\n")

    (define edges-in
      (open-input-file edges-file-import-path))

    (let loop ((id 0)
               (line (read-json edges-in)))
      (when (zero? (modulo id 100000))
        (printf "processing edges line ~s\n" id))
      (cond
        [(eof-object? line)
         (close-input-port edges-in)
         (close-output-port edges-export-out)
         (close-output-port edge-props-out)
         (close-output-port qualified-edge-out)
         (printf "finished processing edges\n\n")]
        [else
         (let ((start (hash-ref line 'subject #f))
               (end (hash-ref line 'object #f)))
           (when (and start end)
             (fprintf edges-export-out "~a\t~a\t~a\n" id start end)
             (let ((predicate (hash-ref line 'predicate ""))
                   (object-aspect (hash-ref line 'object_aspect_qualifier ""))
                   (object-direction (hash-ref line 'object_direction_qualifier ""))
                   (subject (hash-ref line 'subject ""))
                   (object (hash-ref line 'object "")))
               (fprintf qualified-edge-out "~a\t~a\t~a\t~a\t~a\t~a\n"
                        id predicate object-aspect object-direction subject object))
             (let loop-inner (;; all the properties, including the subject and onbject. The order might be various.
                              (propnames (hash-keys line)))
               (when (not (null? propnames))
                 (let* ((propname (car propnames))
                        (value (hash-ref line propname #f)))
                   (unless (equal? "" value)
                     (fprintf edge-props-out "~a\t~a\t~a\n" id propname value)))
                 (loop-inner (cdr propnames)))))
           (loop
            (add1 id)
            (read-json edges-in)))]))))
