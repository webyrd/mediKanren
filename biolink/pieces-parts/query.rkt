#lang racket/base
(provide summarize summarize/assoc query query/graph report/query ranked-paths
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
(define protein   '(;; semmed
                    "biological_entity" ;; with \"Amino Acid, Peptide, or Protein\" in "umls_type_label"
                    ;; robokop
                    ;; orange
                    "(\"gene\" \"genomic entity\")"
                    ;; rtx2
                    "http://w3id.org/biolink/vocab/Protein"
                    ;; "http://w3id.org/biolink/vocab/NamedThing" ;; ugh
                    ))
(define gene-or-protein (set->list (set-union gene protein)))
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

;; TODO: catch naming errors
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
  (cons paths (append csets esets)))

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

(define (curie-norm gs curie)
  (group-curie (findf (lambda (g) (set-member? (group-curies g) curie)) gs)))

(define (report/query q)
  (define paths       (car q))
  (define named-cells (cdr q))
  (define kvs (map (lambda (nc) (cons (car nc) ((cdr nc) 'ref)))
                   named-cells))
  (define csets (filter (lambda (kv) (eq? (cadr kv) 'concept)) kvs))
  (define esets (filter (lambda (kv) (eq? (cadr kv) 'edge))    kvs))
  `((concepts: ,(map (lambda (cset) `(,(car cset) ,(length (cddr cset))))
                     csets))
    (edges: ,(map (lambda (eset) (cons (car eset) (length (cddr eset))))
                  esets))))

(define (ranked-paths q)
  (define paths       (car q))
  (define named-cells (cdr q))
  (define kvs (map (lambda (nc) (cons (car nc) ((cdr nc) 'ref)))
                   named-cells))
  (define csets (filter (lambda (kv) (eq? (cadr kv) 'concept)) kvs))
  (define esets (filter (lambda (kv) (eq? (cadr kv) 'edge))    kvs))
  (define e=>s
    (foldl (lambda (path e=>s)
             (foldl (lambda (edge e=>s)
                      (define ename (cadr  edge))
                      (define sname (car   edge))
                      (hash-set e=>s ename (cddr (assoc sname csets))))
                    e=>s (path->edges path)))
           (hash) paths))
  (define e=>o
    (foldl (lambda (path e=>o)
             (foldl (lambda (edge e=>o)
                      (define ename (cadr  edge))
                      (define oname (caddr edge))
                      (hash-set e=>o ename (cddr (assoc oname csets))))
                    e=>o (path->edges path)))
           (hash) paths))
  (define (augment sgs ogs es)
    (map (lambda (kv)
           (define key (car kv))
           (define es  (cdr kv))
           (define sg (car key))
           (define og (cdr key))
           (list key (augmented-edge-confidence sg og es) es))
         (hash->list
           (foldl (lambda (e acc)
                    (define snorm (curie-norm sgs (cadr (caddr  e))))
                    (define onorm (curie-norm ogs (cadr (cadddr e))))
                    (define key (cons snorm onorm))
                    (define existing (hash-ref acc key #f))
                    (hash-set acc key (if existing (cons e existing) (list e))))
                  (hash) es))))
  (define e=>aes
    (make-immutable-hash
      (map (lambda (kv)
             (define sgs (hash-ref e=>s (car kv)))
             (define ogs (hash-ref e=>o (car kv)))
             (define aes (augment sgs ogs (cddr kv)))
             (cons (car kv) aes))
           esets)))
  (define (path-instances path)
    (let loop ((edges (path->edges path)) (lhs #f))
      (if (null? edges) '(())
        (let ((aes (hash-ref e=>aes (cadar edges))))
          (append* (map (lambda (ae)
                          (define key   (car   ae))
                          (define snorm (car   key))
                          (define onorm (cdr   key))
                          (if (or (not lhs) (equal? lhs snorm))
                            (map (lambda (suffix) (cons ae suffix))
                                 (loop (cdr edges) onorm))
                            '()))
                        aes))))))
  (define path-results
    (map (lambda (path)
           (cons path (sort (map (lambda (p) (cons (path-confidence p) p))
                                 (path-instances path))
                            (lambda (pa pb) (> (car pa) (car pb))))))
         paths))
  path-results)

(define (take/n xs n)
  (if (or (null? xs) (= n 0)) '() (cons (car xs) (take/n (cdr xs) (- n 1)))))

(define (edges/ranked ranked path-pos edge-pos key)
  (define path-report (list-ref ranked path-pos))
  (define instances   (cdr path-report))
  (list-ref (cdr (findf (lambda (i) (equal? (car (list-ref (cdr i) edge-pos)) key))
                        instances))
            edge-pos))

;; TODO: do these values make sense?
(define (base-edge-confidence edge)
  (define (umls? curie) (or (string-prefix? curie "UMLS:")
                            (string-prefix? curie "CUI:")))
  (match edge
    (`(uab-pmi . ,_) 1.0)
    (`(semmed  . ,_) 0.4)
    (`(,_ ,_ (,_ ,subject-curie . ,_) (,_ ,object-curie . ,_) . ,rest)
      (if (or (umls? subject-curie) (umls? object-curie)) 0.4 0.7))))

;; TODO: relevance ranking? drug safety?
(define (edge-confidence edge)
  (define base (base-edge-confidence edge))
  ;; TODO: examine publications, subject/object scores, and other evidence
  base)

(define (augmented-edge-confidence sg og es)
  ;(define (weight-linear+1 n) (+ 1 n))
  (define (weight-exponential n) (expt 2 n))
  (define weight weight-exponential)
  (define support (- 1 (/ 1.0 (weight (length es)))))
  (define base (apply max (map edge-confidence es)))
  (+ base (* (- 1 base) support)))

(define (path-confidence aes) (foldl * 1 (map cadr aes)))

;; TODO: bayesian concept-based or edge-based reinforcement

;; multiple edges (an edge set) between two synonym sets
;;   take max base confidence + accumulate all pubs and evidence?

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


(load-databases #t)

;(displayln "\nRunning query:")
;(define q (time (query/graph
                  ;((S imatinib) ;; represents a single concept
                   ;(X gene)     ;; represents multiple possibilities
                   ;(O asthma))  ;; represents a single concept
                  ;((S->X negatively-regulates)
                   ;(X->O positively-regulates))
                  ;(S S->X X X->O O))))

;(displayln "\nBuilding report:")
;;(pretty-print (summarize/assoc (cdr q)))
;(pretty-print (time (report/query q)))

;(displayln "\nRunning 1-hop rhobtb2 query:")
;(define q (time (query/graph
                  ;((X       #f)
                   ;(rhobtb2 "UMLS:C1425762"))
                  ;((X->rhobtb2 negatively-regulates))
                  ;(X X->rhobtb2 rhobtb2))))

;(displayln "\nBuilding report:")
;(pretty-print (time (report/query q)))

#|
(displayln "\nRunning 2-hop rhobtb2 query:")
(define q (time (query/graph
                  ;; TODO: try constraining categories
                  ((X       #f)
                   (Y       #f)
                   (rhobtb2 "UMLS:C1425762"))
                  ((X->Y       negatively-regulates)
                   (Y->rhobtb2 positively-regulates))
                  (X X->Y Y Y->rhobtb2 rhobtb2))))

(displayln "\nBuilding report:")
(pretty-print (time (report/query q)))

(displayln "\nRanking paths:")
(define ranked (time (ranked-paths q)))
(for ((path-report ranked))
     (define instances (cdr path-report))
     (displayln `(path: ,(length instances) ,(car path-report)))
     (pretty-print (take/n instances 20)))
|#

;; TODO: consolidate synonymous edges

;; TODO: confidence with kg contribution
;; TODO: confidence with semmed score
;; TODO: confidence with counting

;; TODO: include a drug-safe constraint
;; TODO: try constraining by category
;; TODO: try with rtx2


(displayln "\nRunning 2-hop tmprss2 down-up query with concept categories:")
(define q1 (time (query/graph
                  ((X       drug)
                   (Y       gene-or-protein)
                   (tmprss2 "UMLS:C1336641"))
                  ((X->Y       negatively-regulates)
                   (Y->tmprss2 positively-regulates))
                  (X X->Y Y Y->tmprss2 tmprss2))))

(displayln "\nBuilding report:")
(pretty-print (time (report/query q1)))

(displayln "\nRanking paths:")
(define ranked (time (ranked-paths q1)))
(for ((path-report ranked))
     (define instances (cdr path-report))
     (displayln `(path: ,(length instances) ,(car path-report)))
     (pretty-print (map (lambda (pi)
                          (define confidence          (car pi))
                          (define pes        (map car (cdr pi)))
                          (define npes (map (lambda (pe)
                                              (cons (curie->name (car pe))
                                                    (curie->name (cdr pe))))
                                            pes))
                          (list confidence pes npes))
                        (take/n instances 50))))


#|
(displayln "\nRunning 2-hop tmprss2 up-down query with concept categories:")
(define q2 (time (query/graph
                  ((X       drug)
                   (Y       gene-or-protein)
                   (tmprss2 "UMLS:C1336641"))
                  ((X->Y       positively-regulates)
                   (Y->tmprss2 negatively-regulates))
                  (X X->Y Y Y->tmprss2 tmprss2))))

(displayln "\nBuilding report:")
(pretty-print (time (report/query q2)))

(displayln "\nRanking paths:")
(define ranked (time (ranked-paths q2)))
(for ((path-report ranked))
     (define instances (cdr path-report))
     (displayln `(path: ,(length instances) ,(car path-report)))
     (pretty-print (map (lambda (pi)
                          (define confidence          (car pi))
                          (define pes        (map car (cdr pi)))
                          (define npes (map (lambda (pe)
                                              (cons (curie->name (car pe))
                                                    (curie->name (cdr pe))))
                                            pes))
                          (list confidence pes npes))
                        (take/n instances 50))))
|#


#|
(displayln "\nRunning 2-hop rhobtb2 query with concept categories:")
(define q (time (query/graph
                  ((X       drug)
                   (Y       gene-or-protein)
                   (rhobtb2 "UMLS:C1425762"))
                  ((X->Y       negatively-regulates)
                   (Y->rhobtb2 positively-regulates))
                  (X X->Y Y Y->rhobtb2 rhobtb2))))

(displayln "\nBuilding report:")
(pretty-print (time (report/query q)))

(displayln "\nRanking paths:")
(define ranked (time (ranked-paths q)))
(for ((path-report ranked))
     (define instances (cdr path-report))
     (displayln `(path: ,(length instances) ,(car path-report)))
     (pretty-print (map (lambda (pi)
                          (define confidence          (car pi))
                          (define pes        (map car (cdr pi)))
                          (define npes (map (lambda (pe)
                                              (cons (curie->name (car pe))
                                                    (curie->name (cdr pe))))
                                            pes))
                          (list confidence pes npes))
                        (take/n instances 50))))
|#
