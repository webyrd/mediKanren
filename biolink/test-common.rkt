#lang racket
(provide (all-defined-out)
         (all-from-out "common.rkt" "mk-db.rkt"))
(require "common.rkt" "mk-db.rkt")

(define (test-find-Xs)
  (define (decreases? p)
    (member (cddr p) '("negatively_regulates"
                       "prevents"
                       "treats"
                       "disrupts"
                       "increases_degradation_of"
                       "decreases_activity_of"
                       "decreases_expression_of")))
  (define (increases? p)
    (member (cddr p) '("positively_regulates"
                       "produces"
                       "causes"
                       "causes_condition"
                       "causally_related_to"
                       "contributes_to"
                       "gene_associated_with_condition"
                       "gene_mutations_contribute_to"
                       "decreases_degradation_of"
                       "increases_activity_of"
                       "increases_expression_of")))

  (define S (keep 1 (find-concepts #t #f 0 #f  (list "imatin"))))
  (define O (keep 1 (find-concepts #f #t 0 #f  (list "asthma"))))

  (define SP (map (lambda (cp)
                    (define c (car cp))
                    (define pS (cadr cp))
                    (list c (filter decreases? pS) #f))
                  (find-predicates/concepts #t #f S)))
  (define OP (map (lambda (cp)
                    (define c (car cp))
                    (define pO (caddr cp))
                    (list c #f (filter increases? pO)))
                  (find-predicates/concepts #f #t O)))

  (define X (find-Xs SP OP))

  (define x-bcr (car (filter (lambda (x) (equal? (cadddr (car x)) "BCR gene")) X)))

  (displayln 'X:)
  (pretty-print (car x-bcr))
  (newline)
  (displayln 'S-edges:)
  (pretty-print (cadr x-bcr))
  (newline)
  (displayln 'O-edges:)
  (pretty-print (caddr x-bcr)))


(define (predicates/name names)
  (list->set (map (lambda (p) (cons (car p) (cadr p)))
                  (run* (p) (fresh (name)
                              (membero name names)
                              (~predicateo name p))))))

(define decreases
  (predicates/name
    '("negatively_regulates"
      "prevents"
      "treats"
      "disrupts"
      "increases_degradation_of"
      "decreases_activity_of"
      "decreases_expression_of")))

(define increases
  (predicates/name
    '("positively_regulates"
      "produces"
      "causes"
      "causes_condition"
      "causally_related_to"
      "contributes_to"
      "gene_associated_with_condition"
      "gene_mutations_contribute_to"
      "decreases_degradation_of"
      "increases_activity_of"
      "increases_expression_of")))

(define S (keep 1 (find-concepts #t #f 0 #f  (list "imatin"))))
(define O (keep 1 (find-concepts #f #t 0 #f  (list "asthma"))))

(define concept=>set
  (make-immutable-hash
    (map cons '(S O)
         (map (lambda (c) (set (cons (car c) (cadr c)))) (append S O)))))

(define predicate=>cx
  (make-immutable-hash
    (list (cons 'S->X decreases)
          (cons 'X->O increases))))

(pretty-print concept=>set)
(pretty-print predicate=>cx)

(match-define (cons concept-sets edge=>set)
              (find-graph concept=>set (hash) predicate=>cx
                          '((S->X S X) (X->O X O))))

(define (report-concept c)
  (define dbname (car c))
  (define cid (cdr c))
  (car (run* (concept)
         (fresh (cui name catid cat props)
           (== concept `(,dbname ,cid ,cui ,name (,catid . ,cat) ,props))
           (concepto concept)))))
(define (report-edge e)
  (define dbname (car e))
  (define eid (cadr e))
  (define scid (caddr e))
  (define ocid (cadddr e))
  (car (run* (edge)
         (fresh (scui sname scatid scat sprops
                 ocui oname ocatid ocat oprops
                 pid pred eprops)
           (== edge `(,dbname ,eid
                      (,scid ,scui ,sname (,scatid . ,scat) ,sprops)
                      (,ocid ,ocui ,oname (,ocatid . ,ocat) ,oprops)
                      (,pid . ,pred) ,eprops))
           (edgeo edge)))))

(define report-concepts
  (map (lambda (kv)
         (define cname (car kv))
         (cons cname (map report-concept (set->list (cdr kv)))))
       (hash->list concept-sets)))
(define report-edges
  (map (lambda (kv)
         (define ename (car kv))
         (cons ename
               (map report-edge (set->list (cdr kv)))))
       (hash->list edge=>set)))

(displayln "concepts by name:")
(pretty-print report-concepts)
(newline)
(displayln "edges by name:")
(pretty-print report-edges)
