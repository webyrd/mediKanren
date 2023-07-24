#lang racket

(require json
         "transform-utils.rkt")
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

<name-file>.scorededge.tsv
:ID predicate subject object score
|#

(define transform-edge-jsonl
  (lambda (edges-file-import-path
           edge-file-export-path
           edge-props-file-export-path
           scored-edge-file-export-path
           which-kg)

    (printf "transform-edge-jsonl called\n")
    (printf "input edges jsonl: ~s\n" edges-file-import-path)
    (printf "output edge tsv: ~s\n" edge-file-export-path)
    (printf "output edge props tsv: ~s\n" edge-props-file-export-path)
    (printf "output edge tsv: ~s\n" scored-edge-file-export-path)

    (define edges-export-out
      (open-output-file edge-file-export-path))
    (fprintf edges-export-out ":ID\t:START\t:END\n")
    (define edge-props-out
      (open-output-file edge-props-file-export-path))
    (fprintf edge-props-out ":ID\tpropname\tvalue\n")
    (define scored-edge-out
      (open-output-file scored-edge-file-export-path))
    (fprintf scored-edge-out ":ID\tpredicate\tsubject\tobject\tscore\n")

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
         (close-output-port scored-edge-out)
         (printf "finished processing edges\n\n")]
        [else
         (let ((subject (hash-ref line 'subject #f))
               (object (hash-ref line 'object #f)))
           (when (and subject object)
             (fprintf edges-export-out "~a\t~a\t~a\n" id subject object)
             (let* ((predicate (hash-ref line 'predicate #f))
                    (pubs (hash-ref line 'publications #f))
                    (score (cond
                             [(equal? predicate "biolink:subclass_of") 1111]
                             [(not pubs) 0]
                             [(string? pubs) 1]
                             [(list? pubs) #;(build-buckets-with-top 4 (length pubs))
                                           (build-buckets-with-interval (list 0 1 2 3 (cons 4 8) 9)
                                                                        (length pubs))]
                             [else (error "not seen")])))
               (when predicate
                 (fprintf scored-edge-out "~a\t~a\t~a\t~a\t~a\n"
                        id predicate subject object score)))
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
