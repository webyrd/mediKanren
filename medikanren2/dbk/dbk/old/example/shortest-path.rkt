#lang racket/base
(provide m.shortest-path)
(require "base.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define program modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-dbk m.shortest-path
  (link m.base)

  (relation (edge source target distance)
    indexes ((target source)))

  ;; Guard against cyclic data
  (<<= (reachable s t) (edge s t _))
  (<<= (reachable s t) (exist (mid)
                         (edge s mid _) (reachable mid t)))
  (assert (not (exist (x) (reachable x x))))

  ;; This relation will be infinitely large if edge describes a cyclic graph.
  ;; To support cyclic data, we can rewrite this to stratify across time steps,
  ;; stopping when there are no more improvements made during a single tick.
  ;; TODO: It might be possible to avoid stratifying over time by expressing
  ;; shortest-path directly using lattice operations for computing distance,
  ;; allowing recursion within aggregation due to monotonicity.
  (<<= (path s t d) (edge s t d))
  (<<= (path s t (+ d.e d.p))
    (exist (mid)
      (edge s mid d.e)
      (path mid t d.p)))

  (<<= (shortest-path s t distance)
    (=/= distance #f)
    (== distance (merge min #f (query distance (path s t distance)))))

  (<<= (edge s t distance)
    (member (list s t distance)
            '((a b 2)
              (b c 5)
              (b c 1)
              (c d 3)
              (c e 5)
              (d e 1)
              (e f 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define p (process m.shortest-path))

(p 'eval '(query (s t c) (path s t c)))
;; ==>
;; ? TODO

(p 'eval '(query (s t c) (shortest-path s t c)))
;; ==>
;; ? TODO
