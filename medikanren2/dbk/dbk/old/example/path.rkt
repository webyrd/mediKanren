#lang racket/base
(provide m.path m.edge.acyclic m.edge.cycles)
(require "base.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define program modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-dbk m.path
  (relation (edge source target)
    indexes ((target source)))

  (<<= (path s t) (edge s t))
  (<<= (path s t) (exist (mid)
                    (edge s mid) (path mid t))))

(define-dbk m.edge.acyclic
  (link m.base)
  (<<+ (edge s t) (member (list s t)
                          '((1 2)
                            (2 4)
                            (1 3)
                            (3 5)
                            (2 6)
                            (3 6)
                            (6 4)))))

(define-dbk m.edge.cycles
  (link m.base)
  (<<+ (edge s t) (member (list s t)
                          '((1 1)
                            (5 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define p (process m.path))

;; edge relation is empty
(p 'eval '(query (s t) (path s t)))
;; ==>
;; ()

;; insert acyclic edge data
(p 'program-set! (link m.path m.edge.acyclic))
(p 'tick!)
(p 'program-set! m.path)

(p 'eval '(query (s t) (path s t)))
;; ==>
;; ? TODO

;; insert extra edges that form cycles
(p 'program-set! (link m.path m.edge.cycles))
(p 'tick!)
(p 'program-set! m.path)

(p 'eval '(query (s t) (path s t)))
;; ==>
;; ? TODO
