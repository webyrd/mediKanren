#lang racket/base

(provide
 chem-affects-gene
 chem-affects-gene-entries
 chem-affects-gene-TSV)

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level-multi-db.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers-multi-db.rkt"
 "../../../../medikanren2/neo/neo-server/neo-server-utils.rkt"
 "../utils.rkt"
 json
 racket/format
 racket/list
 racket/match
 racket/set
 racket/pretty
 racket/string)

;; finds a list of compounds that affect a provided set of genes
(define (chem-affects-gene gene-list)
  (time (remove-duplicates
         (query:X->Known
          (set->list
           (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
            '("biolink:ChemicalEntity")))
          (set->list
           (get-non-deprecated/mixin/abstract-ins-and-descendent-predicates*-in-db
            '("biolink:affects")))
          (set->list (curie-synonyms-and-descendents gene-list))))))

(define header
  (list "chemical CURIE" "chemical name" "predicate description" "object CURIE" "object name" "NCBITaxon" "PUBMEDs" "source"))

(define (create-entry result)
  (match result
    [`(,subj-curie ,pred ,obj-curie . ,props)
     (list
       subj-curie
       (concept->name subj-curie)
       (or (get-assoc "description" props)
           (get-assoc "predicate_label" props)
           (let ((qp (get-assoc "qualified_predicate" props)))
             (if qp
                 (string-append
                  qp
                  " "
                  (or (get-assoc "object_direction_qualifier" props) "")
                  " "
                  (or (get-assoc "object_aspect_qualifier" props) ""))
                 #f))
           pred)
       obj-curie
       (concept->name obj-curie)
       (let ((taxon (get-assoc "NCBITaxon" props)))
         (cond
           ((equal? "9606" taxon) (string-append taxon " (Homo sapiens)"))
           ((equal? "10090" taxon) (string-append taxon " (Mus musculus)"))
           (else (or taxon "N/A"))))
       (string-join (get-pubs props) ",")
       (get-primary-knowledge-source props))]))

(define (chem-affects-gene-entries gene-list)
  (cons header
        (remove-duplicates
         (map create-entry
              (chem-affects-gene gene-list)))))

(define (chem-affects-gene-TSV file-name gene-list)
  (write-answers-to-tsv
   file-name
   (chem-affects-gene-entries gene-list)))
