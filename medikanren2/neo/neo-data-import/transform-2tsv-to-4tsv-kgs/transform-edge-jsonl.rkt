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


(define counters (hash))
(define build-buckets-with-distribution
  (lambda (predicate score buckets-needed start-bucket-numbers)
    (set! counters
          (hash-update counters
                       predicate
                       (lambda(pred-h)
                         (hash-update pred-h score add1 0))
                       (hash)))
    (let* ((edge-count (hash-ref (hash-ref counters predicate) score))
           (num-buckets (hash-ref (hash-ref buckets-needed predicate) score))
           (start-bucket-number (hash-ref (hash-ref start-bucket-numbers predicate) score))
           (specific-bucket (modulo (- edge-count 1) (max num-buckets 1)))
           (bucket-assignment (+ start-bucket-number specific-bucket)))
      bucket-assignment)))  

(define transform-edge-jsonl
  (lambda (edges-file-import-path
           bucket-needed-path
           start-bucket-numbers-path
           edge-file-export-path
           edge-props-file-export-path
           scored-edge-file-export-path
           which-kg)

    (define buckets-needed (build-pred-score-amount-hash bucket-needed-path))
     
    (define start-bucket-numbers (build-pred-score-amount-hash start-bucket-numbers-path))

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
         (printf "finished processing edges\n")
         (printf "the current counters ~s\n\n" counters)]
        [else
         (let ((subject (hash-ref line 'subject #f))
               (object (hash-ref line 'object #f))
               (predicate (hash-ref line 'predicate #f))
               (robokop-primary_knowledge_source (hash-ref line 'primary_knowledge_source #f)))
           (if (equal? robokop-primary_knowledge_source "infores:text-mining-provider-targeted")
               (loop id (read-json edges-in))
               (begin
                 (when (and subject object predicate)
                   (fprintf edges-export-out "~a\t~a\t~a\n" id subject object)
                   (let* ((pubs (hash-ref line 'publications #f))
                          (score (cond
                                   [(not pubs) 0]
                                   [(string? pubs) 1]
                                   [(list? pubs) (length pubs)]
                                   [else (error "not seen")]))
                          (bucket-num
                           (cond
                             [(equal? predicate "biolink:subclass_of") 1111]
                             [(equal? predicate "biolink:gene_product_of") 1112]
                             [else (build-buckets-with-distribution
                                    predicate score buckets-needed start-bucket-numbers)])))
                     (fprintf scored-edge-out "~a\t~a\t~a\t~a\t~a\n"
                              id predicate subject object bucket-num)
                     (let loop-inner (;; all the properties, including the subject and onbject. The order might be various.
                                      (propnames (hash-keys line)))
                       (when (not (null? propnames))
                         (let* ((propname (car propnames))
                                (value (hash-ref line propname))
                                (value (if (hash? value)
                                           (jsexpr->string value)
                                           value)))
                           (unless (or (equal? "" value) (equal? 'null value))
                             (cond
                               ((equal? propname 'qualified_object_aspect)
                                (fprintf edge-props-out "~a\tobject_aspect_qualifier\t~a\n" id value))
                               ((equal? propname 'qualified_object_direction)
                                (fprintf edge-props-out "~a\tobject_direction_qualifier\t~a\n" id value))
                               ((equal? propname 'biolink:primary_knowledge_source)
                                (fprintf edge-props-out "~a\tprimary_knowledge_source\t~a\n" id value))
                               (else
                                (fprintf edge-props-out "~a\t~a\t~a\n" id propname value)))))
                         (loop-inner (cdr propnames))))
                     (fprintf edge-props-out "~a\tmediKanren-score\t~a\n" id score)))
                 (loop
                  (add1 id)
                  (read-json edges-in)))))]))))
