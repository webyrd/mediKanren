#lang racket/base
(provide trapi-response)
(require
  "../../medikanren2/lw-reasoning.rkt"
  racket/file racket/function racket/list racket/hash
  (except-in racket/match ==)
  racket/port
  racket/pretty
  racket/runtime-path
  racket/string
  json
  )

(define diabetes "DOID:9351")
(define diabetes2 "DOID:9352")

;; Reminder on 'semantic web'-style relations, defined in `medikanren2/common.rkt`

;; `is-a` is a short-hand for the "category" property
(run* c (is-a diabetes c))
(run* c (cprop diabetes "category" c))

;; and `triple` abstracts away the edge-id on triple relations
(run 1 (s p o) (triple s p o)
(run 1 (s p o) 
  (fresh (eid)
    (edge eid s o)
    (eprop eid "predicate" p)))

;; Query subclasses in the Biolink Ontology directly using `subclass-of`, `subclass-of*`, `subclass-of+`
(run* c (subclass-of+ c "biolink:regulates"))

;; Subclass relations on concept category
(run* c (is-a diabetes c))
(run* (c c^) (is-a/subclass* diabetes c c^))

;; Look for drugs that ameliorate (or a subclass thereof) diabetes
(run* (drug p) (triple/subclass drug "biolink:ameliorates" p diabetes))

;; ... or only subclasses thereof (mostly for testing)
(run* (drug p) (triple/subclass+ drug "biolink:ameliorates" p diabetes))

;; ... and drugs that ameliorate (or a subclass thereof) diabetes (or a subclass thereof)
(run* (drug p^ diabetes^) 
  (triple/subclass drug "biolink:ameliorates" p^ diabetes^)
  (subclass-of* diabetes^ diabetes))

;; ... there is a single relation that does the same thing, but currently it is very slow,
;; so explicitly defining the subclass relations as in the previous example is much quicker
(run 1 (drug drug^ p diabetes^) 
  (triple/reasoning drug drug^ "biolink:ameliorates" p diabetes diabetes^))

(define gene "NCBIGene:7422")

(run* o (triple gene "biolink:regulates" o))

(run 10 (o p) (triple/subclass+ gene "biolink:regulates" p o))


