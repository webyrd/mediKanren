#lang racket
(require "transform-utils.rkt"
         "rtx-kg2-bucket-table.rkt"
         "../../neo-data/raw_downloads_from_kge_archive/text-mining-apr-2-2024/text-mining-bucket-distribution.rkt")
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
                         (hash-update pred-h score add1))))
    (let* ((edge-count (hash-ref (hash-ref counters predicate) score))
           (num-buckets (hash-ref (hash-ref buckets-needed predicate) score))
           (start-bucket-number (hash-ref (hash-ref start-bucket-numbers predicate) score))
           (specific_bucket (modulo (- edge-count 1) num-buckets))
           (bucket-assignment (+ start-bucket-number specific_bucket)))
      bucket-assignment)))

(define transform-edge-tsv
  (lambda (edges-file-import-path
           edge-file-export-path
           edge-props-file-export-path
           scored-edge-file-export-path
           which-kg)
    
    (when (eq? which-kg 'text-mining)
      (set! counters text-mining-counters))

    (printf "transform-edge-tsv version 1.0\n")
    
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
          (printf "processing edges line ~s\n" id)
          (printf "the current counters ~s\n" counters))

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
                                      predicate score text-mining-buckets-needed text-mining-start-bucket-numbers)))
                    (list predicate subject object score bucket-num))]
                 [(eq? which-kg 'rtx-kg2)
                  (let* ((pubs (list-ref line 6))
                         (predicate (list-ref line 15))
                         (subject (list-ref line 16))
                         (object (list-ref line 17))
                         (pub-len (if (or (equal? pubs "")
                                          (equal? pubs "PMID:"))
                                    0
                                    (length (string-split pubs "; "))))
                         (score (if (and (equal? predicate "biolink:gene_associated_with_condition")
                                         (= pub-len 30))
                                    (cond
                                      [(zero? (modulo id 100)) ((hash-ref kg2-buckets predicate) (+ pub-len 2))]
                                      [(zero? (modulo id 40)) ((hash-ref kg2-buckets predicate) (+ pub-len 1))]
                                      [else ((hash-ref kg2-buckets predicate) pub-len)])

                                    #;(build-buckets-with-top 3 pub-len)
                                    #;(build-buckets-with-interval (list 0 1 2 (cons 3 5) (cons 6 29) 30)
                                                                   pub-len)
                                    #;(build-buckets-with-interval (list 0 1 2 (cons 3 5) (cons 6 29) 30 (cons 31 66) 67)
                                                                   pub-len)
                                    ((hash-ref kg2-buckets predicate) pub-len)
                                    )))
                    (list predicate subject object score score))]
                 ))
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
             (loop
              (add1 id)
              (read-line edges-in 'any)))))))))
