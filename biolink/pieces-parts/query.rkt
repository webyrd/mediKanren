#lang racket/base
(provide base-confidence summarize
         (all-from-out "../common.rkt" "../mk-db.rkt" "propagator.rkt"))
(require "../common.rkt" "../mk-db.rkt" "propagator.rkt"
         racket/list (except-in racket/match ==) racket/set racket/string)

;; TODO: do these values make sense?
(define (base-confidence edge)
  (define (umls? curie) (or (string-prefix? curie "UMLS:")
                            (string-prefix? curie "CUI:")))
  (match edge
    (`(uab-pmi . ,_) 1.0)
    (`(semmed  . ,_) 0.5)
    (`(,_ ,_ (,_ ,subject-curie . ,_) (,_ ,object-curie . ,_) . ,rest)
      (if (or (umls? subject-curie) (umls? object-curie)) 0.5 0.75))))


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

;;(define (path*->edges paths cnames enames)
  ;;(when (null? paths) (error "no paths were provided"))
  ;;(unless (list? paths) (error "paths must be given as a list:" paths))
  ;;(for-each
    ;;(lambda (path)
      ;;(unless (list? path) (error "path must be a list:" path))
      ;;(unless (<= 3 (length path))
        ;;(error "path must contain at least one edge triple:" path))
      ;;(let loop ((parts path))
        ;;(cond ((null? parts) (error "missing concept at end of path:" path))
              ;;((not (member (car parts) cnames))
               ;;(error "unknown path concept:"
                      ;;path `(unknown-concept: ,(car parts))
                      ;;`(valid-concepts: ,cnames)))
              ;;((and (pair? (cdr parts)) (not (member (cadr parts) enames)))
               ;;(error "unknown path edge:"
                      ;;path `(unknown-edge: ,(cadr parts))
                      ;;`(valid-edges: ,enames)))
              ;;((pair? (cdr parts)) (loop (cddr parts))))))
    ;;paths)
  ;;(define (path->edges path)
    ;;(if (null? (cdr path)) '()
      ;;(cons (list (cadr path) (car path) (caddr path))
            ;;(path->edges (cddr path)))))
  ;;(append* (map path->edges paths)))



;(define (path->edges path)
  ;(if (and (pair? path) (null? (cdr path))) '()
    ;(cons (take path 3) (path->edges (drop path 2)))))

;(define (run-query concepts edge-predicates paths)
  ;(define known   (make-immutable-hash
                    ;(map (lambda (kv)
                           ;(cons (car kv) (concept-synonyms (list (cdr kv)))))
                         ;(filter (lambda (kv) (not (pair? (cdr kv))))
                                 ;concepts))))
  ;(define unknown (make-immutable-hash
                    ;(map (lambda (kv)
                           ;(cons (car kv)
                                 ;(if (cdr kv) (find-exact-categories (cdr kv))
                                   ;(run* (c) (categoryo c)))))
                         ;(filter (lambda (kv) (pair? (cdr kv))) concepts))))
  ;(define edges   (append* (map path->edges paths)))
  ;(define s=>e    (make-immutable-hash
                    ;(map (lambda (e) (cons (car   e)  (cadr e))) edges)))
  ;(define o=>e    (make-immutable-hash
                    ;(map (lambda (e) (cons (caddr e)  (cadr e))) edges)))
  ;(define e=>s    (make-immutable-hash
                    ;(map (lambda (e) (cons (cadr  e)   (car e))) edges)))
  ;(define e=>o    (make-immutable-hash
                    ;(map (lambda (e) (cons (cadr  e) (caddr e))) edges)))
  ;(define e=>p    (make-immutable-hash
                    ;(map (lambda (kv) (cons (car kv) (find-exact-predicates
                                                       ;(cdr kv))))
                         ;edge-predicates)))


  ;;; start with known, and explore, and find fixed point
  ;(let loop ((pending known))
  ;)


;;; results for unknowns must be grouped by synonym before proceeding
;;; each edge must have at least one member of a synonym group participating in it
;;;   otherwise the synonym group is removed from consideration

;(define-syntax-rule (query/graph ((concept-name initial) ...)
                                 ;((edge-name predicate) ...)
                                 ;path ...)
  ;(run-query `((concept-name . ,initial)   ...)
             ;`((edge-name    . ,predicate) ...)
             ;'(path ...)))

;(query/graph
  ;((S imatinib) ;; represents a single concept
   ;(X gene)     ;; represents multiple possibilities
   ;(O asthma))  ;; represents a single concept
  ;((S->X negatively-regulates)
   ;(X->O positively-regulates))
  ;(S S->X X X->O O))

;; TODO:
;query/report

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
