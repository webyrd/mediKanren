#lang racket/base

(provide query)

(require
 "query-low-level-temporary-kg.rkt"
  "../neo-reasoning/neo-biolink-reasoning.rkt"
)

(require
 racket/list
 racket/set
 racket/string
)

(define (entity-query-unknown? x)
  (cond
   ((string=? x "")
    #t)
   ((empty? x)
    #t)
   ((string-prefix? x "biolink:")
    #t)
   (else #f)))

(define (to-unknown x)
  (cond
    ((string=? x "")
     #f)
    ((empty? x)
    #f)
    ((string-prefix? x "biolink:")
     (set->list
      (get-non-deprecated-mixed-ins-and-descendent-classes*
       (list x))))
    (else
     (list x))))

(define (synonyms x)
  ;; TODO
  (list x))

(define (curies-in-db-safe x)
  (and x (curies-in-db x)))

(define (query subject predicate object)
  (displayln (list subject predicate object))
  (let ((predicates
         (set->list
          (if (string-contains? predicate " ")
              (string-split predicate)
              (if (string=? "" predicate)
                  all-predicates
                  (get-non-deprecated-mixed-ins-and-descendent-predicates* (if (string=? "biolink:treats" predicate) '("biolink:treats" "biolink:treats_or_applied_or_studied_to_treat") (list predicate))))))))
    (let ((q
           (cond
	     ((entity-query-unknown? subject)
              (displayln "X->Known")
              (list query:X->Known (to-unknown subject) predicates (synonyms object)))
	     ((entity-query-unknown? object)
              (displayln "Known->X")
              (list query:Known->X (synonyms subject) predicates (to-unknown object)))
	     (else
              (displayln "Known->Known")
              (list query:Known->Known (list subject) predicates (list object))))))
      (let ((r (apply (car q) (map curies-in-db-safe (cdr q)))))
      ;;(set! r (cleanup r))
      r))))

