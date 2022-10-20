#lang racket/base

(provide
 curie->synonyms-in-db
 curies->synonyms-in-db
 ;;
 all-predicates-in-db
 all-classes-in-db
 ;;
 get-predicate-descendents-in-db
 get-class-descendents-in-db
 ;;
 get-predicate-descendents*-in-db
 get-class-descendents*-in-db
 ;;
 get-descendent-curies-in-db
 get-descendent-curies*-in-db

 iota
 pretty-print-json-string
 take-at-most
 )
(require
 "../dbKanren/dbk/database.rkt"
 "../neo-low-level/query-low-level.rkt"
 "../neo-low-level/synonym-low-level.rkt"
 "../neo-reasoning/neo-biolink-reasoning.rkt"
 racket/set)

(define (curie->synonyms-in-db curie)
  (filter curie-in-db? (curie->synonyms curie)))

(define (curies->synonyms-in-db curies)
  (filter curie-in-db? (curies->synonyms curies)))

(define all-predicates-in-db
  (list->set
    (filter curie-in-db?
            (set->list all-predicates))))
(define all-classes-in-db
  (list->set
    (filter curie-in-db?
            (set->list all-classes))))

(define (get-predicate-descendents-in-db pred)
  (list->set
    (filter curie-in-db?
            (set->list (get-predicate-descendents pred)))))
(define (get-class-descendents-in-db class)
  (list->set
    (filter curie-in-db?
            (set->list (get-class-descendents class)))))

(define (get-predicate-descendents*-in-db preds)
  (list->set
    (filter curie-in-db?
            (set->list (get-predicate-descendents* preds)))))
(define (get-class-descendents*-in-db classes)
  (list->set
    (filter curie-in-db?
            (set->list (get-class-descendents* classes)))))


(define (get-descendent-curies-in-db curie)
  (get-descendent-curies*-in-db (list curie)))

(define (get-descendent-curies*-in-db curies)
  (set-fixed-point
   (list->set
    (map car
         (query:X->Known
          #f
          (list "biolink:subclass_of")
          curies)))
   (lambda (new-curies)
     (list->set
      (map car
           (query:X->Known
            #f
            (list "biolink:subclass_of")
            (set->list new-curies)))))))

(define (iota n)
  (define (iter i)
    (if (>= i n)
        '()
        (cons i (iter (+ 1 i)))))
  (iter 0))

(define (pretty-print-json-string json-string port)
  (define len (string-length json-string))
  (define (display-indent-spaces n port)
    (let loop ([i 0])
      (cond
        [(< i n)
         (display #\space port)
         (loop (add1 i))]
        [else (void)])))
  (let loop ([i 0]
             [indent 0]
             [in-quote #f])
    (cond
      [(< i len)
       (let ((c (string-ref json-string i)))
         (case c
           [(#\")
            (display c port)
            (loop (add1 i) indent (not in-quote))]
           ;;
           [(#\:)
            (display c port)
            (unless in-quote
              (display #\space port))
            (loop (add1 i) indent in-quote)]
           ;;
           [(#\,)
            (display c port)
            (unless in-quote
              (newline port)
              (display-indent-spaces indent port))
            (loop (add1 i) indent in-quote)]
           ;;
           [(#\{ #\[)
            (display c port)
            (if in-quote
                (loop (add1 i) indent in-quote)                
                (let ((indent (add1 indent)))
                  (newline port)
                  (display-indent-spaces indent port)
                  (loop (add1 i) indent in-quote)))]
           ;;
           [(#\} #\])
            (if in-quote
                (begin
                  (display c port)
                  (loop (add1 i) indent in-quote))
                (let ((indent (sub1 indent)))
                  (newline port)
                  (display-indent-spaces indent port)
                  (display c port)
                  (loop (add1 i) indent in-quote)))]
           ;;
           [else
            (display c port)
            (loop (add1 i) indent in-quote)]))]
      [else (void)])))

(define (take-at-most ls n)
  (cond
    [(<= n 0) '()]
    [(null? ls) '()]
    [else
     (cons (car ls)
           (take-at-most (cdr ls) (sub1 n)))]))
