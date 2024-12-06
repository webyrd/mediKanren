#lang racket

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level-multi-db.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers-multi-db.rkt"
 "../../../../medikanren2/neo/neo-command-line-interface/utils.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers-multi-db.rkt"
 )

(define file-name "gene_list_medikanren_merged.csv")
(define in (open-input-file file-name))

(define genes
  (let* ((header (read-line in 'any))
         (header (string-split header "," #:trim? #f)))
    (let loop ((line-str (read-line in 'any))
               (genes '()))
      (cond
        ((eof-object? line-str)
         (close-input-port in)
         (printf "finished processing gene curies")
         genes)
        (else
         (let* ((line (string-split line-str "," #:trim? #f))
                (curie (cadr line))
                (curie (string-append "ENSEMBL:" curie)))
           (loop (read-line in 'any) (cons curie genes))))))))

(define genes+ (curie->gene/protein-conflation (curies->synonyms genes)))

(define (write-answers edge* path)
  (write-list-to-tsv
   '("subject_curie"
     "subject_name"
     "subject_categories"
     "gene_curie"
     "gene_name"
     "predicate"
     "qualified_predicate"
     "object_aspect_qualifier"
     "object_direction_qualifier"
     "publications"
     "publication_count"
     "edge_source"
     )
   (map (lambda (e)
          (match e
            [`(,sub ,pred ,obj . ,prop)
             (let* ((qualified-pred (get-assoc "qualified_predicate" prop))
                    (object-aspect-qualifier (get-assoc "object_aspect_qualifier" prop))
                    (object-direction-qualifier (get-assoc "object_direction_qualifier" prop))
                    (source (get-assoc "primary_knowledge_source" prop))
                    (publications (get-publications prop))
                    (publication-count (length publications))
                    )
               (list
                sub
                (curie->name-remember sub)
                (curie->categories-remember sub)
                obj
                (curie->name-remember obj)
                pred
                (if qualified-pred qualified-pred "not apply")
                (if object-aspect-qualifier object-aspect-qualifier "not apply")
                (if object-direction-qualifier object-direction-qualifier "not apply")
                publications
                publication-count
                source
                ))]
            [else '()]))
        (remove-duplicates edge*))
   path))

(define (mediKanren-activates curie* path)
  (write-answers
   (filter
    activate-edge-filter
   (query:X->Known
    (set->list (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db '("biolink:ChemicalOrDrugOrTreatment")))
    '("biolink:has_increased_amount"
      "biolink:affects"
      "biolink:produces"
      "biolink:regulates"
      "biolink:enables"
      ;"biolink:treats"
      "biolink:positively_correlated_with"
      ;"biolink:ameliorates"
      )
    curie*))
   path))

(define (mediKanren-inhibits curie* path)
  (write-answers
   (filter
    inhibite-edge-filter
    (query:X->Known
     (set->list (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db '("biolink:ChemicalOrDrugOrTreatment")))
     '("biolink:prevents"
       "biolink:affects"
       "biolink:regulates"
       "biolink:has_decreased_amount"
       "biolink:disrupts"
       "biolink:negatively_correlated_with")
     curie*))
   path))

;(time (mediKanren-activates genes+ "drugmix_activate_genes.tsv"))
;(time (mediKanren-inhibits genes+ "drugmix_inhibit_genes.tsv"))