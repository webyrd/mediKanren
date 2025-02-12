#lang racket

(require json
         "../transform-2tsv-to-4tsv-kgs/transform-utils.rkt")

(provide get-pub-dist-tsv
         get-pred-tsv
         get-pub-dist-jsonl
         get-pubs-counts-jsonl
         get-pred-jsonl)


(define get-pub-dist-tsv
  (lambda (tsv factor?)
    (define pub (make-hash))
    (define in (open-input-file tsv))
    (define header (efficient-no-trim-tab-string-split (read-line in 'any)))
    (define (helper i)
      (when (zero? (modulo i 1000000))
          (printf "processing edges line ~s\n" i))
      (define line-str (read-line in 'any))
      (when (not (eof-object? line-str))
        (define line (efficient-no-trim-tab-string-split line-str))

        (define pub-idx (find-index header "publications"))
        (define pred-idx (find-index header "predicate"))
        (define factor-idx (if factor? (find-index header "score") #f))
        (define pubs (list-ref line pub-idx))
        (define pred (list-ref line pred-idx))
        (define factor (if factor? (list-ref line factor-idx) 1))

        (if (equal? pubs "")
            (hash-update! pub pred (lambda (oldh) (hash-update oldh 0 add1 0)) (hash))
            (let ((pub-len (length (string-split pubs "|"))))
              (hash-update! pub
                            pred
                            (lambda (oldh)
                              (hash-update oldh
                                           (exact-round (* factor pub-len))
                                           add1
                                           0))
                            (hash))))
        (helper (add1 i))))
    (helper 1)
    (close-input-port in)
    (hash->list pub)))

(define get-pred-tsv
  (lambda (jsonl)
    (define pred-t (make-hash))
    (define in (open-input-file jsonl))
    (define (helper i)
      (when (zero? (modulo i 1000000))
          (printf "processing edges line ~s\n" i))
      (define line-str (read-line in 'any))
      (when (not (eof-object? line-str))
        (define line (efficient-no-trim-tab-string-split line-str))
        #|
        (define pred (list-ref line 1)) ;text mining
        (define qualified-pred (list-ref line 3)) ;text mining
        (define object-direction-qualifier (list-ref line 9)) ;text mining
        (define object-aspect-qualifier (list-ref line 8)) ;text mining
        |#
        (define pred (list-ref line 15)) ;rtx-kg2
        (define qualified-pred (list-ref line 10)) ;rtx-kg2
        (define object-direction-qualifier (list-ref line 9)) ;rtx-kg2
        (define object-aspect-qualifier (list-ref line 8)) ;rtx-kg2
        (when pred
            (hash-update! pred-t
                          pred
                          (lambda (accu)
                            (remove-duplicates
                             (cons
                              (list qualified-pred object-aspect-qualifier object-direction-qualifier)
                              accu)))
                          '()))
        (helper (add1 i))))
    (helper 1)
    (close-input-port in)
    (hash->list pred-t)))

(define get-pub-dist-jsonl
  (lambda (jsonl)
    (define pub (make-hash))
    (define in (open-input-file jsonl))
    (define (helper i)
      (when (zero? (modulo i 1000000))
          (printf "processing edges line ~s\n" i))
      (define line (read-json in))
      (when (not (eof-object? line))
        (define pubs (hash-ref line 'publications #f))
        (define pred (hash-ref line 'predicate #f))
        (if pubs
            (let ((pub-len (cond
                             [(string? pubs) 1]
                             [(list? pubs) (length pubs)]
                             [else (display pubs) "not seen"])))
              (hash-update! pub
                            pred
                            (lambda (oldh)
                              (hash-update oldh
                                           pub-len 
                                           add1
                                           0))
                            (hash)))
            (hash-update! pub pred (lambda (oldh) (hash-update oldh 0 add1 0)) (hash)))
        (helper (add1 i))))
    (helper 1)
    (close-input-port in)
    (hash->list pub)))

(define get-pubs-counts-jsonl
  (lambda (jsonl pub-sym)
    (define pub-count-dict (make-hash))
    (define in (open-input-file jsonl))
    (define helper
      (lambda ()
        (define json (read-json in))
        (when (not (eof-object? json))    
          (define pubs (hash-ref json pub-sym #f))
          (if pubs
              (let ((pub-len (cond
                               [(string? pubs)
                                 (max (length (string-split pubs "|")) (length (string-split pubs)))]
                               [(list? pubs) (length pubs)]
                               [else (display pubs) "not seen"])))
                (hash-update! pub-count-dict pub-len add1 0))
              (hash-update! pub-count-dict 0 add1 0))
          (helper))))
    (helper)
    (close-input-port in)
    (hash->list pub-count-dict)))

(define get-pred-jsonl
  (lambda (jsonl)
    (define pred-t (make-hash))
    (define in (open-input-file jsonl))
    (define (helper i)
      (when (zero? (modulo i 1000000)) ;; print the lines and number after 73000000
          (printf "processing edges line ~s\n" i))
      (define line (read-json in))
      (when (not (eof-object? line))
        (define pred (hash-ref line 'predicate ""))
        (define qualified-pred (hash-ref line 'qualified_predicate ""))
        (define object-direction-qualifier (hash-ref line 'object_direction_qualifier ""))
        (define object-aspect-qualifier (hash-ref line  'object_aspect_qualifier ""))
        (when pred
            (hash-update! pred-t
                          pred
                          (lambda (accu)
                            (remove-duplicates
                             (cons
                              (list qualified-pred object-aspect-qualifier object-direction-qualifier)
                              accu)))
                          '()))
        (helper (add1 i))))
    (helper 1)
    (close-input-port in)
    (hash->list pred-t)))


