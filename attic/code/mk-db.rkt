#lang racket/base
(provide
  (all-from-out "mk.rkt")
  concepto
  fuzzy-concepto
  concept-semtypeo
  cuio
  cui*o
  edgeo
  membero
  not-membero
  edge-subjecto
  edge-objecto
  concept-not-too-generalo
  edge-not-too-generalo
  path-not-too-generalo
  )

(require
  "db.rkt"
  "edge.rkt"
  "mk.rkt"
  racket/stream
  )

;; list membership
(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        [(== x y)]
        [(=/= x y) (membero x rest)]))))

;; list non-membership
(define not-membero
  (lambda (x ls)
    (conde
      [(== '() ls)]
      [(fresh (y rest)
         (== `(,y . ,rest) ls)
         (=/= x y)
         (not-membero x rest))])))

(define (ground-cui c) (and (pair? c) (number? (car c)) (car c)))

(define (concept->list c) (vector->list (concept-pretty c)))

(define (edge->list e)
  (list (concept->list (hash-ref cui=>concept (edge-src e)))
        (concept->list (hash-ref cui=>concept (edge-dst e)))
        (hash-ref id=>predicate (edge-predicate e))
        (hash-ref id=>semtype (edge-src-type e))
        (hash-ref id=>semtype (edge-dst-type e))
        (edge-pub-info e)))

;; f and xs must be ground.
(define (mapo f xs x)
  (if (null? xs) fail
    (conde
      ((== (f (car xs)) x))
      ((mapo f (cdr xs) x)))))

(define (concepto c)
  (project (c)
    (if (ground-cui c)
      (== (concept->list (hash-ref cui=>concept (car c))) c)
      (mapo concept->list concept* c))))

(define (fuzzy-concepto name c)
  (mapo concept->list (fuzzy-name->concept* concept* name #t) c))

(define (concept-semtypeo c semtype)
  (fresh (cui name semtype*)
    (== `(,cui ,name ,semtype*) c)
    (membero semtype semtype*)))

(define (cuio c cui) (fresh (c-rest) (== `(,cui . ,c-rest) c)))

(define (cui*o c cui*)
  (fresh (cui a d)
    (== `(,a . ,d) cui*)
    (cuio c cui)
    (conde
      ((== cui a))
      ((=/= cui a) (cui*o c d)))))

(define (edgeo edge)
  (fresh (subject object predicate subject-type object-type pubref)
    (== `(,subject ,object ,predicate ,subject-type ,object-type ,pubref) edge)
    (project (subject object predicate subject-type object-type pubref)
      (let* ((subject-cui (ground-cui subject))
             (object-cui (ground-cui object))
             (predicate-id (and (not (var? predicate))
                                (hash-ref predicate=>id predicate)))
             (subject-stid (and (not (var? subject-type))
                                (hash-ref semtype=>id subject-type)))
             (object-stid (and (not (var? object-type))
                               (hash-ref semtype=>id object-type)))
             (p? (lambda (src dst src-type dst-type)
                   (lambda (e)
                     (and (or (not src) (= src (edge-src e)))
                          (or (not dst) (= dst (edge-dst e)))
                          (or (not predicate-id)
                              (= predicate-id (edge-predicate e)))
                          (or (not src-type) (= src-type (edge-src-type e)))
                          (or (not dst-type)
                              (= dst-type (edge-dst-type e))))))))
        (cond
          (subject-cui
            (let loop
              ((e* (subject->edge*/stream
                     subject-cui
                     (p? subject-cui object-cui subject-stid object-stid))))
              (if (stream-empty? e*) fail
                (conde
                  ((== (edge->list (stream-first e*)) edge))
                  ((loop (stream-rest e*)))))))
          (object-cui
            (let loop
              ((e* (object->edge*/stream
                     object-cui
                     (p? object-cui subject-cui object-stid subject-stid))))
              (if (stream-empty? e*) fail
                (conde
                  ((== (edge->list (edge-reverse (stream-first e*))) edge))
                  ((loop (stream-rest e*)))))))
          ((and predicate-id subject-stid object-stid)
           (let loop
             ((e* (predicate->edge*/stream
                    predicate-id subject-stid object-stid (lambda (_) #t))))
             (if (stream-empty? e*) fail
               (conde
                 ((== (edge->list (stream-first e*)) edge))
                 ((loop (stream-rest e*)))))))
          (else
            (let loop
              ((e* (edge*/stream
                     (p? subject-cui object-cui subject-stid object-stid))))
              (if (stream-empty? e*) fail
                (conde
                  ((== (edge->list (stream-first e*)) edge))
                  ((loop (stream-rest e*))))))))))))

(define (edge-subjecto e s)
  (fresh (o p sty oty rs)
    (== `(,s ,o ,p ,sty ,oty ,rs) e)))

(define (edge-objecto e o)
  (fresh (s p sty oty rs)
    (== `(,s ,o ,p ,sty ,oty ,rs) e)))

(define (concept-not-too-generalo c)
  (project (c)
    (if (<= 1000 (count-ISA (ground-cui c)))
      fail
      succeed)))

(define (edge-not-too-generalo e)
  (fresh (s o p sty oty rs)
    (== `(,s ,o ,p ,sty ,oty ,rs) e)
    (project (s o)
      (fresh ()
        (concept-not-too-generalo s)
        (concept-not-too-generalo o)))))

(define (path-not-too-generalo p)
  (conde
    ((== '() p))
    ((fresh (a d)
       (== `(,a . ,d) p)
       (edge-not-too-generalo a)
       (path-not-too-generalo d)))))
