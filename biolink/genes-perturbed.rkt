#lang racket/base
(provide (all-defined-out) (all-from-out "common.rkt"))
(require "common.rkt" "repr.rkt"
         (except-in racket/match ==) racket/set racket/string)

(define increases
  (find-predicates '(;; common
                     "causes" "positively_regulates"
                     ;; semmed
                     "produces"
                     ;; robokop
                     "decreases_degradation_of"
                     "increases_secretion_of"
                     "increases_transport_of"
                     "increases_activity_of"
                     "increases_synthesis_of"
                     "increases_expression_of"
                     "positively_regulates__entity_to_entity"
                     )))

(define decreases
  (find-predicates '(;; common
                     "prevents" "negatively_regulates"
                     ;; semmed
                     ;; robokop
                     "decreases_secretion_of"
                     "decreases_transport_of"
                     "decreases_activity_of"
                     "decreases_synthesis_of"
                     "decreases_expression_of"
                     "increases_degradation_of"
                     "negatively_regulates__entity_to_entity"
                     "disrupts"
                     )))

(define (find-exact-categories names)
  (run* (cat) (fresh (db catid name)
                (membero name names)
                (== cat `(,db ,catid . ,name))
                (categoryo cat))))

(define gene (find-exact-categories '(;; semmed
                                      "gene"
                                      ;; robokop
                                      "(\"named_thing\" \"gene\")")))
(define drug (find-exact-categories '(;; semmed
                                      "chemical_substance"
                                      ;; robokop
                                      "(\"named_thing\" \"chemical_substance\")")))

(define (file->strings filename)
  (call-with-input-file
    filename
    (lambda (in) (let loop ()
                   (define line (read-line in))
                   (if (eof-object? line) '()
                     (cons (string-trim line) (loop)))))))

(define (xref-curies curies)
  (time (run* (c) (fresh (xref)
                    (membero xref curies)
                    (xref-concepto xref c)))))

(define hgnc-ids (file->strings "hgnc-ids.txt"))
(printf "HGNC gene total: ~s\n" (length hgnc-ids))

;; This will only work with robokop.
;(define human-genes (time (run* (c) (~cui*-concepto hgnc-ids c))))

;(define drugs
  ;(time (run* (c) (fresh (db dbcat)
                    ;(membero dbcat drug)
                    ;(category-concepto dbcat c)))))
;(printf "Total drug count: ~s\n" (length drugs))

;; This will work for either semmed or robokop, but isn't as complete.
(define genes
  (time (run* (c) (fresh (db dbcat)
                    (membero dbcat gene)
                    (category-concepto dbcat c)))))
(printf "Total gene count: ~s\n" (length genes))

;; TODO: This set-based deduping is due to one duplicate semmed entry.
;; We need to fix this by re-indexing xrefs.
(define human-genes (set->list (list->set (xref-curies hgnc-ids))))
(define human-genes-alt
  (time (filter (lambda (g)
                  (ormap (lambda (xref) (string-prefix? xref "HGNC:"))
                         (concept->xrefs (list->vector (cddr g)))))
                genes)))
(printf "HGNC genes found: ~s ~s ~s\n" (length human-genes) (length human-genes-alt)
        (set-count (set-intersect (list->set human-genes) (list->set human-genes-alt))))

(define (%hgnc-total count)
  (/ count (exact->inexact (length hgnc-ids))))
(define (%hgnc-found count)
  (/ count (exact->inexact (length human-genes))))

;; This also works (and is faster) for robokop:
;(define human-genes
  ;(time (filter (lambda (g) (string-prefix? (caddr g) "HGNC:"))
                ;genes)))

(define (perturb-1-hop direction)
  (match-define
    (list name=>concepts _)
    (time (run/graph ((D drug)
                      (G human-genes))
                     ((D->G direction))
                     (D D->G G))))
  (list->set (hash-ref name=>concepts 'G)))

(define (perturb-next-hop X direction)
  (set-intersect
    (list->set human-genes)
    (list->set
      (time (run* (concept)
              (fresh (x c cid cui name cat props db eid pred eprops)
                (membero `(,db . ,cat) gene)
                (== c `(,cid ,cui ,name ,cat ,props))
                (membero `(,db . ,x) X)
                (edgeo `(,db ,eid ,x ,c ,pred ,eprops))
                (membero `(,db . ,pred) direction)
                (=/= x c)
                (== concept `(,db . ,c))))))))

;; 1-hop

(define increased-1-hop (perturb-1-hop increases))
(printf "increased 1-hop: ~s\n" (set-count increased-1-hop))
(define decreased-1-hop (perturb-1-hop decreases))
(printf "decreased 1-hop: ~s\n" (set-count decreased-1-hop))
(define perturbed-1-hop (set-union increased-1-hop decreased-1-hop))
(printf "perturbed 1-hop: ~s ~s ~s\n"
        (set-count perturbed-1-hop)
        (%hgnc-found (set-count perturbed-1-hop))
        (%hgnc-total (set-count perturbed-1-hop)))

;; 2-hop

;; increase
(define inc&inc-2-hop (perturb-next-hop (set->list increased-1-hop) increases))
(printf "inc&inc 2-hop: ~s\n" (set-count inc&inc-2-hop))
(define dec&dec-2-hop (perturb-next-hop (set->list decreased-1-hop) decreases))
(printf "dec&dec 2-hop: ~s\n" (set-count dec&dec-2-hop))
(define increased-2-hop (set-union inc&inc-2-hop dec&dec-2-hop))
(define increased<=2-hop (set-union increased-1-hop increased-2-hop))
(printf "increased 2-hop: ~s ~s\n"
        (set-count increased-2-hop)
        (set-count increased<=2-hop))

;; decrease
(define dec&inc-2-hop (perturb-next-hop (set->list decreased-1-hop) increases))
(printf "dec&inc 2-hop: ~s\n" (set-count dec&inc-2-hop))
(define inc&dec-2-hop (perturb-next-hop (set->list increased-1-hop) decreases))
(printf "inc&dec 2-hop: ~s\n" (set-count inc&dec-2-hop))
(define decreased-2-hop (set-union dec&inc-2-hop inc&dec-2-hop))
(define decreased<=2-hop (set-union decreased-1-hop decreased-2-hop))
(printf "decreased 2-hop: ~s ~s\n"
        (set-count decreased-2-hop)
        (set-count decreased<=2-hop))

(define perturbed-2-hop (set-union increased-2-hop decreased-2-hop))
(define perturbed<=2-hop (set-union increased<=2-hop decreased<=2-hop))
(printf "perturbed 2-hop: ~s ~s ~s ~s ~s ~s\n"
        (set-count perturbed-2-hop)
        (%hgnc-found (set-count perturbed-2-hop))
        (%hgnc-total (set-count perturbed-2-hop))
        (set-count perturbed<=2-hop)
        (%hgnc-found (set-count perturbed<=2-hop))
        (%hgnc-total (set-count perturbed<=2-hop)))

;; 3-hop

;; increase
(define inc&inc-3-hop (perturb-next-hop (set->list (set-subtract increased-2-hop increased-1-hop)) increases))
(printf "inc&inc 3-hop: ~s\n" (set-count inc&inc-3-hop))
(define dec&dec-3-hop (perturb-next-hop (set->list (set-subtract decreased-2-hop decreased-1-hop)) decreases))
(printf "dec&dec 3-hop: ~s\n" (set-count dec&dec-3-hop))
(define increased-3-hop (set-union inc&inc-3-hop dec&dec-3-hop))
(define increased<=3-hop (set-union increased<=2-hop increased-3-hop))
(printf "increased 3-hop: ~s ~s\n"
        (set-count increased-3-hop)
        (set-count increased<=3-hop))

;; decrease
(define dec&inc-3-hop (perturb-next-hop (set->list (set-subtract decreased-2-hop decreased-1-hop)) increases))
(printf "dec&inc 3-hop: ~s\n" (set-count dec&inc-3-hop))
(define inc&dec-3-hop (perturb-next-hop (set->list (set-subtract increased-2-hop increased-1-hop)) decreases))
(printf "inc&dec 3-hop: ~s\n" (set-count inc&dec-3-hop))
(define decreased-3-hop (set-union dec&inc-3-hop inc&dec-3-hop))
(define decreased<=3-hop (set-union decreased<=2-hop decreased-3-hop))
(printf "decreased 3-hop: ~s ~s\n"
        (set-count decreased-3-hop)
        (set-count decreased<=3-hop))

(define perturbed-3-hop (set-union increased-3-hop decreased-3-hop))
(define perturbed<=3-hop (set-union increased<=3-hop decreased<=3-hop))
(printf "perturbed 3-hop: ~s ~s ~s ~s ~s ~s\n"
        (set-count perturbed-3-hop)
        (%hgnc-found (set-count perturbed-3-hop))
        (%hgnc-total (set-count perturbed-3-hop))
        (set-count perturbed<=3-hop)
        (%hgnc-found (set-count perturbed<=3-hop))
        (%hgnc-total (set-count perturbed<=3-hop)))

;; Only do this when generating Robokop data.
;(call-with-output-file
  ;"hgnc-robokop.txt"
  ;(lambda (out)
    ;(for-each
      ;(lambda (c) (fprintf out "~a\n" (caddr c)))
      ;(set->list perturbed<=3-hop))))

;; Only do this comparison when we have Robokop data.
;(define hgnc-robokop-ids (file->strings "hgnc-robokop.txt"))
;(define hgnc-robokop-concepts (list->set (xref-curies hgnc-robokop-ids)))
;(define hgnc-robokop-missing (- (length hgnc-robokop-ids) (set-count hgnc-robokop-concepts)))
;(define hgnc-common (set-intersect hgnc-robokop-concepts perturbed<=3-hop))
;(define hgnc-combined (set-union hgnc-robokop-concepts perturbed<=3-hop))
;(define hgnc-total-perturbed (+ hgnc-robokop-missing (set-count hgnc-combined)))
;(define extra         (- (set-count perturbed<=3-hop)
                         ;(set-count hgnc-common)))
;(define extra-robokop (- (length hgnc-robokop-ids)
                         ;(set-count hgnc-common)))

;(printf "overlap with Robokop: this=~s robokop=~s; common=~s ~s\n"
        ;(set-count perturbed<=3-hop)
        ;(set-count hgnc-robokop-concepts)
        ;(set-count hgnc-common)
        ;(%hgnc-total (set-count hgnc-common)))

;(printf "missing: ~s\n" hgnc-robokop-missing)

;(printf "extra and combined contributions: extra=~s ~s; extra-robokop=~s ~s; combined=~s ~s\n"
        ;extra                (%hgnc-total extra)
        ;extra-robokop        (%hgnc-total extra-robokop)
        ;hgnc-total-perturbed (%hgnc-total hgnc-total-perturbed))
