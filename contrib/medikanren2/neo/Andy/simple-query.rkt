#lang racket/base

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level-multi-db.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers-multi-db.rkt"
 json
 racket/format
 racket/list
 racket/match
 racket/set
 racket/pretty
 racket/string)

(define (concept->name curie)
  (caddar (remove-duplicates (filter (lambda (cl)
                                      (and (equal? (car cl) curie)
                                           (equal? (cadr cl) "name")))
                                    (query:Concept (list curie))))))

(define (one-hop-results->jsexpr results)
  (let loop ((results results)
             (edges (set))
             (nodes (set)))
    (cond
      ((null? results) (hasheq 'nodes (set->list nodes) 'edges (set->list edges)))
      (else (let ((res (car results)))
              (match res
                (`(,subj ,pred ,obj . ,props)
                 (loop (cdr results)
                       (set-add edges (hasheq 'source subj 'target obj))
                       (set-add (set-add nodes (hasheq 'id subj 'label (concept->name subj)))
                                (hasheq 'id obj 'label (concept->name obj)))))))))))



(define regulates-EGFR
  (time (query:X->Known
         #f
         (set->list
          (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
           '("biolink:regulates")))
         (set->list
          (get-descendent-curies*-in-db
           (curies->synonyms-in-db (list "HGNC:3236")))))))

(map
 (lambda (edge)
   (match edge
     (`(,subj ,pred ,obj . ,props)
      (list (concept->name subj)
            pred
            (concept->name obj)
            props))))
 regulates-EGFR)

#|
This is all that is required:
{
  "nodes": [
    { "id": "node1", "label": "Node 1" },
    { "id": "node2", "label": "Node 2" },
    { "id": "node3", "label": "Node 3" }
  ],
  "edges": [
    { "source": "node1", "target": "node2" },
    { "source": "node2", "target": "node3" }
  ]
}
I use the curie as the ID and the name as the label for nodes and get the categories as well.
Then also get the predicate and qualifiers,  from the edge, but have not used that much. So, if It hard to break out then I can go without for now.
|#


(let ((op (open-output-file "results.json" #:mode 'text #:exists 'replace)))
  (write-json (one-hop-results->jsexpr regulates-EGFR) op)
  (close-output-port op))



#|
Gene1 - any node - Disease 1

probably should exclude `biolink:same_as` as a predicate
|#

(define (gene->any->disease gene disease)
  (time (query:Known->X->Known
         (set->list
          (get-descendent-curies*-in-db
           (curies->synonyms-in-db (list gene))))
         (set->list
          (get-non-deprecated/mixin/absreact-ins-and-descendent-predicates*-in-db
           '("biolink:affects"  "biolink:interacts_with")))
         #f
         (set->list
          (get-non-deprecated/mixin/absreact-ins-and-descendent-predicates*-in-db
           '("biolink:affects"  "biolink:interacts_with")))
         (set->list
          (get-descendent-curies*-in-db
           (curies->synonyms-in-db (list disease)))))))

;; BRCA2 and breast cancer
(define BRCA2->any->breast-cancer
  (gene->any->disease "HGNC:1101" "MONDO:0007254"))
