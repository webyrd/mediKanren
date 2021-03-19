#lang racket/base
(provide json->se se->json jsexpr->se se->jsexpr)
(require json)

(define (json->se s)  (jsexpr->se (string->jsexpr s)))
(define (se->json se) (jsexpr->string (se->jsexpr se)))

(define (jsexpr->se j)
  (cond ((hash? j) (define kvs (hash->list j))
                   (map cons (map car kvs) (map jsexpr->se (map cdr kvs))))
        ((pair? j) (list->vector (map jsexpr->se j)))
        (else      j)))

(define (se->jsexpr se)
  (cond ((pair? se)   (make-immutable-hasheq
                        (map cons (map car se) (map se->jsexpr (map cdr se)))))
        ((vector? se) (map se->jsexpr (vector->list se)))
        (else         se)))
