#lang racket/base
(provide summarize summarize/assoc query query/graph report/paths
         positively-regulates negatively-regulates drug-safe
         gene drug disease phenotype
         (all-defined-out)
         (all-from-out "../common.rkt" "../mk-db.rkt"
                       "propagator.rkt" "synonymize.rkt"))
(require "../common.rkt" "../mk-db.rkt" "propagator.rkt" "synonymize.rkt"
         racket/list (except-in racket/match ==) racket/pretty
         racket/set racket/string)

#|
;; ** rtx2 neutral predicates -- do we want to include these somewhere? **

"regulates_expression_of"
"regulates_activity_of"
"targets"
|#

(define positively-regulates '(;; common
                               "causes" "positively_regulates" "produces"
                               ;; robokop
                               "decreases_degradation_of"
                               "increases_secretion_of"
                               "increases_transport_of"
                               "increases_activity_of"
                               "increases_synthesis_of"
                               "increases_expression_of"
                               "positively_regulates__entity_to_entity"
                               ;; orange
                               "causes_condition"
                               "contributes_to"
                               ;; rtx2
                               "directly_positively_regulates"
                               "stimulates"
                               "activator"
                               "augments"
                               "induces"
                               "cause_of"
                               "causes_or_contributes_to"
                               "contributes_to_condition"
                               ))
(define negatively-regulates '(;; common
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
                               ;; orange
                               ;; rtx2
                               "directly_negatively_regulates"
                               "inhibits"
                               "inhibitor"
                               "channel_blocker"
                               "treats"
                               "disrupts"
                               "may_inhibit_effect_of"
                               "may_prevent"
                               "may_treat"
                               ))
(define drug-safe            '("clinically_tested_approved_unknown_phase"
                               "clinically_tested_terminated_phase_2"
                               "clinically_tested_terminated_phase_3"
                               "clinically_tested_terminated_phase_2_or_phase_3"
                               "clinically_tested_withdrawn_phase_3"
                               "clinically_tested_withdrawn_phase_2_or_phase_3"
                               "clinically_tested_withdrawn_phase_2"
                               "clinically_tested_suspended_phase_2"
                               "clinically_tested_suspended_phase_3"
                               "clinically_tested_suspended_phase_2_or_phase_3"
                               ))
(define gene      '(;; semmed
                    "gene"
                    ;; robokop
                    "(\"named_thing\" \"gene\")"
                    ;; orange
                    "(\"gene\")"
                    "(\"genomic entity\" \"gene\")"
                    "(\"gene\" \"genomic entity\")"
                    ;; rtx2
                    "http://w3id.org/biolink/vocab/Gene"
                    ))
(define drug      '(;; semmed
                    "chemical_substance"
                    ;; robokop
                    "(\"named_thing\" \"chemical_substance\")"
                    ;; orange?
                    ;; rtx2
                    "http://w3id.org/biolink/vocab/Drug"
                    "http://w3id.org/biolink/vocab/ChemicalSubstance"
                    "http://w3id.org/biolink/vocab/Chemical_Substance"
                    ))
(define disease   '(;; semmed
                    "disease_or_phenotypic_feature"
                    ;; robokop
                    "(\"named_thing\" \"disease\")"
                    "(\"named_thing\" \"genetic_condition\" \"disease\")"
                    "(\"named_thing\" \"disease\" \"phenotypic_feature\")"
                    "(\"named_thing\" \"genetic_condition\" \"disease\" \"phenotypic_feature\")"
                    ;; orange
                    "(\"disease\")"
                    ;; rtx2
                    "http://w3id.org/biolink/vocab/Disease"
                    ))
(define phenotype '(;; semmed
                    "phenotypic_feature"
                    ;; robokop
                    "(\"named_thing\" \"phenotypic_feature\")"
                    ;; orange
                    "(\"phenotypic feature\")"
                    ;; rtx2
                    "http://w3id.org/biolink/vocab/PhenotypicFeature"
                    "http://w3id.org/biolink/vocab/Phenotypic_Feature"
                    ))

(define imatinib "UMLS:C0935989")
(define asthma   "UMLS:C0004096")

(define (path->edges path)
  (if (and (pair? path) (null? (cdr path))) '()
    (cons (take path 3) (path->edges (drop path 2)))))

(define (query concepts edge-predicates paths)
  (define edges (append* (map path->edges paths)))
  (define csets (map cons (map car concepts)
                     (map (lambda (c)
                            (cond ((string? c) (concept/curie c))
                                  ((not c)     (concept/any))
                                  (else        (concept/category c))))
                          (map cdr concepts))))
  (define esets
    (map (lambda (e) (let ((s (car e)) (p (cadr e)) (o (caddr e)))
                       (cons p (edge/predicate (cdr (assoc p edge-predicates))
                                               (cdr (assoc s csets))
                                               (cdr (assoc o csets))))))
         edges))
  (cons edges (append csets esets)))

(define (summarize-edge es)
  (map (lambda (e) (list (car e) (cadr e) (car (cddddr e))
                         (cons (cadr (caddr  e)) (caddr (caddr  e)))
                         (cons (cadr (cadddr e)) (caddr (cadddr e)))))
       es))
(define (summarize-concept gs)
  (map (lambda (curies) (map (lambda (c) (take c 4))
                             (find-concepts #t curies)))
       (map set->list (map group-curies gs))))
(define (summarize cell)
  (match (cell 'ref)
    (`(edge    . ,es) `(edge    . ,(summarize-edge    es)))
    (`(concept . ,gs) `(concept . ,(summarize-concept gs)))
    (v                v)))
(define (summarize/assoc named-cells)
  (map cons (map car named-cells) (map summarize (map cdr named-cells))))

(define-syntax-rule (query/graph ((concept-name initial) ...)
                                 ((edge-name predicate) ...)
                                 path ...)
  (let ((q (query `((concept-name . ,initial)   ...)
                  `((edge-name    . ,predicate) ...)
                  '(path ...))))
    (run!)
    q))

(define (curie-norm curie)
  (define curies (set->list (curie-synonyms (list curie))))
  (foldl (lambda (a b) (if (string<? a b) a b)) (car curies) (cdr curies)))

(define (report/paths paths q)
  (define edges       (car q))
  (define named-cells (cdr q))
  (define kvs (map (lambda (nc) (cons (car nc) ((cdr nc) 'ref)))
                   named-cells))
  (define csets (filter (lambda (kv) (eq? (cadr kv) 'concept)) kvs))
  (define esets (filter (lambda (kv) (eq? (cadr kv) 'edge))    kvs))
  (define path-results
    (map (lambda (path)
           (cons path
                 (let loop ((edges (path->edges path)) (lhs #f))
                   (if (null? edges) '(())
                     (let* ((ename (cadar  edges))
                            (es    (cddr (assoc ename esets))))
                       (append*
                         (map (lambda (e)
                                (define snorm (curie-norm (cadr (caddr  e))))
                                (define onorm (curie-norm (cadr (cadddr e))))
                                (if (or (not lhs) (string=? lhs snorm))
                                  (map (lambda (suffix)
                                         (append (if lhs (list e onorm)
                                                   (list snorm e onorm))
                                                 suffix))
                                       (loop (cdr edges) onorm))
                                  '()))
                              es)))))))
         paths))
  ;; TODO: confidence ranking of paths
  ;; multiply per-edge confidences
  ;;   KG/CURIE base confidence
  ;;   publication count
  ;;   synonymous edge count
  ;; TODO: relevance ranking? drug safety?
  `((paths: ,path-results)
    (concepts:
      ,(map (lambda (cset)
              `(,(car cset)
                 ,(length (cddr cset))
                 ,(map (lambda (g) (cons (group-curie g)
                                         (set-count (group-curies g))))
                       (cddr cset))))
            csets))
    (edges: ,(map (lambda (eset)
                    (cons (car eset) (length (cddr eset))))
                  esets))))

;; TODO: do these values make sense?
(define (base-edge-confidence edge)
  (define (umls? curie) (or (string-prefix? curie "UMLS:")
                            (string-prefix? curie "CUI:")))
  (match edge
    (`(uab-pmi . ,_) 1.0)
    (`(semmed  . ,_) 0.5)
    (`(,_ ,_ (,_ ,subject-curie . ,_) (,_ ,object-curie . ,_) . ,rest)
      (if (or (umls? subject-curie) (umls? object-curie)) 0.5 0.75))))

;(define (edge-confidence edge)
  ;)

;(define (path-confidence edges)
  ;)

;; concept confidences: 1.0 for given and synonyms, 0.5 for 1-hop xref

;; TODO: improve edge confidence with publications
;; TODO: adjust base concept confidence with subject/object scores in rtx2

;; multiple edges (an edge set) between two synonym sets
;;   take max base confidence + accumulate all pubs and evidence?

;augment concept sets, partition by xref depth

;find edges

;fill in unknown concepts

;repeat until no more unknown concepts or missing edges


;compute edge confidences:
  ;(f (* edge-base subject-xref-base object-xref-base) pubs evidence)

;;; multiply sub-path confidence between this concept and every other known concept node
;;; intuition: if there is no path to an anchor node, that's confidence 0, and
;;; multiplied by anything is 0, meaning this concept should not be included
;compute concept confidences:
  ;(* concept-xref-base
     ;(apply * (map (apply max (map (path-confidence path)
                                   ;(paths-to known-concept)))
                   ;known-concepts)))

;;; to rank edges, first rank solved unknown concepts
;;; return best concept's best paths to each anchor node
;;; gradually swap in lower-confidence edges and adjust best concept's confidence downward to match
;;; if best concept is no longer best, swap in new best, and repeat until all edges are returned
;;; ... or just let the upstream reporter report edges specific to selected unknown results



;(load-databases #t)
;(define q (time (query/graph
                  ;((S imatinib) ;; represents a single concept
                   ;(X gene)     ;; represents multiple possibilities
                   ;(O asthma))  ;; represents a single concept
                  ;((S->X negatively-regulates)
                   ;(X->O positively-regulates))
                  ;(S S->X X X->O O))))

;;(pretty-print (summarize/assoc (cdr q)))
;(pretty-print (report/paths '((S S->X X X->O O)) q))
