#lang racket
(require
  "db.rkt"
  "concept.rkt"
  "edge.rkt"
  )

(define dfo* (fuzzy-name*->concept* concept* '("fish" "oil") #t))
(define raynaud* (fuzzy-name->concept* concept* "raynaud" #t))

(define epoprostenol* (fuzzy-name->concept* concept* "epoprostenol" #t))
(define prostaglandin* (fuzzy-name->concept* concept* "prostaglandin" #t))

;; This seems to be the one we're looking for.
(define prostaglandins (fuzzy-name->concept* concept* "prostaglandins" #t))

(define platelet* (fuzzy-name*->concept* concept* '("platelet" "aggregation") #t))

;; Why are these two edge groups so different from what we expect to see?
;; https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4031661/figure/F1/
(define dfo-epoprostenol-edge* (direct-edge* dfo* epoprostenol*))
(define dfo-prostaglandin-edge* (direct-edge* dfo* prostaglandin*))

;; These groups have edges we're expecting.
(define epoprostenol-raynaud-edge* (direct-edge* epoprostenol* raynaud*))
(define prostaglandin-raynaud-edge* (direct-edge* prostaglandin* raynaud*))
(define prostaglandin-epoprostenol-edge* (direct-edge* prostaglandin* epoprostenol*))
(define prostaglandin-platelet-edge* (direct-edge* prostaglandin* platelet*))

;; Expected edges are missing here.
(define platelet-raynaud-edge* (direct-edge* platelet* raynaud*))

;; No direct edges here.
(define dfo-raynaud-edge* (direct-edge* dfo* raynaud*))
