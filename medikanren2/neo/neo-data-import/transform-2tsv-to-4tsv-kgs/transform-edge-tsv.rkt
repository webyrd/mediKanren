#lang racket
(require "transform-utils.rkt")
(provide transform-edge-tsv)

#|
Output edge and edge-props file formats:

<name-file>.edge.tsv
:ID :START :END

<name-file>.edge-props.tsv
:ID propname value

<name-file>.qualifiededge.tsv
:ID predicate object-aspect object-direction subject object

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

(define transform-edge-tsv
  (lambda (edges-file-import-path
           bucket-needed-path
           start-bucket-numbers-path
           edge-file-export-path
           edge-props-file-export-path
           scored-edge-file-export-path
           which-kg)
    
    (define buckets-needed (build-pred-score-amount-hash bucket-needed-path))

    (define start-bucket-numbers (build-pred-score-amount-hash start-bucket-numbers-path))

    (printf "transform-edge-tsv\n")
    (printf "transform-edge-tsv called\n")
    (printf "input edges tsv: ~s\n" edges-file-import-path)
    (printf "output edge tsv: ~s\n" edge-file-export-path)
    (printf "output edge props tsv: ~s\n" edge-props-file-export-path)
    (printf "output scored edge tsv: ~s\n" scored-edge-file-export-path)
    
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
    
    (let* ((header (read-line edges-in 'any))
           (header (string-split header "\t" #:trim? #f)))
      (let loop ((id 0)
                 (line-str (read-line edges-in 'any)))
        (when (zero? (modulo id 100000))
          (printf "processing edges line ~s\n" id))
        (cond
          ((eof-object? line-str)
           (close-input-port edges-in)
           (close-output-port edges-export-out)
           (close-output-port edge-props-out)
           (close-output-port scored-edge-out)
           (printf "finished processing edges\n\n")
           (printf "the current counters ~s\n" counters))
          (else
           (let ((line (efficient-no-trim-tab-string-split line-str)))
             (match-define (list predicate subject object score bucket-num)
               (cond
                 [(eq? which-kg 'text-mining)
                  (let* ((predicate (list-ref line 1))
                         (subject (list-ref line 0))
                         (object (list-ref line 2))
                         (pubs (list-ref line 17))
                         (pub-len (if (equal? pubs "")
                                    0
                                    (length (string-split pubs "|"))))
                         (TM-score (string->number (list-ref line 15)))
                         (score (exact-round (* TM-score pub-len)))
                         (bucket-num (build-buckets-with-distribution
                                      predicate score buckets-needed start-bucket-numbers)))
                    (list predicate subject object score bucket-num))]
                 [(eq? which-kg 'rtx-kg2)
                  (let* ((predicate (list-ref line (find-index header "predicate")))
                         (subject (list-ref line (find-index header "subject")))
                         (object (list-ref line (find-index header "object")))
                         (pubs (list-ref line (find-index header "publications")))
                         (score (if (equal? pubs "")
                                    0
                                    (length (string-split pubs "|"))))
                         (bucket-num (cond
                                       [(equal? predicate "biolink:subclass_of") 1111]
                                       [(equal? predicate "biolink:gene_product_of") 1112]
                                       [else (build-buckets-with-distribution
                                              predicate score buckets-needed start-bucket-numbers)])))
                    (list predicate subject object score bucket-num))]
                 [else (error "unknown KG")]))
             (unless (or (string=? "" subject) (string=? "" object))
               (fprintf edges-export-out "~a\t~a\t~a\n" id subject object)
               (unless (string=? "" predicate)
                 (fprintf scored-edge-out "~a\t~a\t~a\t~a\t~a\n" id predicate subject object bucket-num)))
               
             (let loop-inner ((props line) ;; all the properties, including the subject and object
                              (headers header))
               (when (not (null? props))
                 (unless (string=? "" (car props))
                   (let ((propname (car headers))
                         (value (car props)))
                     (fprintf edge-props-out "~a\t~a\t~a\n" id propname value)))
                 (loop-inner (cdr props) (cdr headers))))
             (fprintf edge-props-out "~a\tmediKanren-score\t~a\n" id score)
             #;(fprintf edge-props-out "~a\tprimary_knowledge_source\tinfores:text-mining-provider-targeted\n" id)
             (loop
              (add1 id)
              (read-line edges-in 'any)))))))))
