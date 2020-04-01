#lang racket
(provide HGNC-CURIE->synonymized-concepts
         curie-aliases curie-synonyms curie->name curie->concepts
         curie-synonyms/names
         (all-from-out "../common.rkt" "../mk-db.rkt"))
(require "../common.rkt" "../mk-db.rkt")

(define (curie->name? curie)
  (define cs (find-concepts #t (list curie)))
  (and (pair? cs) (cadddr (car cs))))
(define (curie->name curie)
  (define name? (curie->name? curie))
  (or name? (ormap curie->name? (set->list (curie-synonyms curie)))))
(define (curie->concepts curie)
  (find-concepts #t (set->list (curie-synonyms curie))))
(define (curie-synonyms/names curie)
  ;; (printf "curie-synonyms/names for curie ~s\n" curie)
  (define css (set->list (time (curie-synonyms curie))))
  ;; (printf "css = ~s\n" css)
  (map cons css (time (map curie->name css))))

(define (curie-aliases curies)
  (foldl (lambda (c acc)
           (cond ((string-prefix? c "UMLS:")
                  (cons (string-replace c "UMLS:" "CUI:") acc))
                 ((string-prefix? c "CUI:")
                  (cons (string-replace c "CUI:" "UMLS:") acc))
                 ((string-prefix? c "NCI:")
                  (cons (string-replace c "NCI:" "NCIT:") acc))
                 ((string-prefix? c "NCIT:")
                  (cons (string-replace c "NCIT:" "NCI:") acc))
                 (else acc)))
         curies curies))

(define (curie-UMLS-gene? c)
  (and (string-prefix? (caddr c) "UMLS:")
       (string=? (cdar (cddddr c)) "gene")))
(define (curie-UMLS-drug? c)
  (and (string-prefix? (caddr c) "UMLS:")
       (string=? (cdar (cddddr c)) "chemical_substance")))
(define (curie-xref-gene? curie)
  (ormap (lambda (pre) (string-prefix? curie pre))
         '("NCI:" "HGNC:")))
(define (curie-xref-drug? curie)
  (ormap (lambda (pre) (string-prefix? curie pre))
         '("MTHSPL:" "NDFRT:" "RXNORM:" "CHEMBL:")))
(define ((curie-prefix? prefix) c) (string-prefix? (caddr c) prefix))
(define curie-NCIT? (curie-prefix? "NCIT:"))
(define curie-CUI?  (curie-prefix? "CUI:"))
(define (any? x) #t)

(define (curie-synonyms curie)
  (define max-synonyms 100)
  (define same-as     (find-exact-predicates (list "equivalent_to" "encodes")))
  (define subclass-of (find-exact-predicates (list "subclass_of")))
  (define xref        (find-exact-predicates (list "xref")))
  (define preds/filters
    (list (list same-as     any?        any?)
          ;(list subclass-of curie-NCIT? curie-NCIT?)
          (list xref        curie-NCIT? curie-CUI?)))
  (define (connect-edges cs)
    (append*
      (map (lambda (p/fs)
             (match-define (list preds s? o?) p/fs)
             (append (filter o? (run* (x)
                                  (fresh (s o p db eid erest)
                                    (membero `(,db . ,s) (filter s? cs))
                                    (== x `(,db . ,o))
                                    (membero `(,db . ,p) preds)
                                    (edgeo `(,db ,eid ,s ,o ,p . ,erest)))))
                     (filter s? (run* (x)
                                  (fresh (s o p db eid erest)
                                    (membero `(,db . ,o) (filter o? cs))
                                    (== x `(,db . ,s))
                                    (membero `(,db . ,p) preds)
                                    (edgeo `(,db ,eid ,s ,o ,p . ,erest)))))))
           preds/filters)))
  (define (xref-forward cs0)
    (define cs-gene (filter (lambda (c) (curie-UMLS-gene? c)) cs0))
    (define xs-gene (filter curie-xref-gene?
                            (run* (x) (fresh (c)
                                        (membero c cs-gene)
                                        (xref-concepto x c)))))
    (define cs-drug (filter (lambda (c) (curie-UMLS-drug? c)) cs0))
    (define xs-drug (filter curie-xref-drug?
                            (run* (x) (fresh (c)
                                        (membero c cs-drug)
                                        (xref-concepto x c)))))
    (append xs-gene xs-drug))
  (define (xref-backward ids0)
    (define xs-gene
      (filter (lambda (c) (curie-xref-gene? c)) ids0))
    (define cs-gene (filter curie-UMLS-gene?
                            (run* (c) (fresh (x)
                                        (membero x xs-gene)
                                        (xref-concepto x c)))))
    (define xs-drug
      (filter (lambda (c) (curie-xref-drug? c)) ids0))
    (define cs-drug (filter curie-UMLS-drug?
                            (run* (c) (fresh (x)
                                        (membero x xs-drug)
                                        (xref-concepto x c)))))
    (map caddr (append cs-gene cs-drug)))

  (define ids (curie-aliases (list curie)))
  (let retry ((synonym-concepto? #t) (xref-concepto? #t))
    (let loop ((ids0 ids) (synonym-ids (list->set ids)))
      (if (< max-synonyms (set-count synonym-ids))
        (cond (synonym-concepto? (retry #f #t))
              (xref-concepto?    (retry #f #f))
              (else              (list->set ids)))
        ;; (printf "ids0 = ~s\n" ids0)
        (let* ((cs0 (find-concepts #t ids0))
               (ids (if synonym-concepto?
                      (run* (curie)
                        (fresh (c)
                          (membero c cs0)
                          (synonym-concepto curie c)))
                      '()))
               (cs  (if synonym-concepto?
                      (run* (c)
                        (fresh (curie)
                          (membero curie ids0)
                          (synonym-concepto curie c)))
                      '()))
               (ids (append ids (map caddr cs)
                            (map caddr (connect-edges cs0))
                            (if xref-concepto?
                              (append (xref-forward cs0) (xref-backward ids0))
                              '())))
               (ids (set-subtract (list->set (curie-aliases ids))
                                  synonym-ids)))
          (if (set-empty? ids) synonym-ids
            (loop (set->list ids) (set-union synonym-ids ids))))))))


(define DEBUG-SYNONYMIZE #f)

(define human-gene/protein-suffix-ls
  (list " protein, human"
        " gene"
        " Gene"
        " wt Allele"
        " (human)"))

(define animal-model/bacteria/plant-gene/protein-suffix-ls
  (list " protein, mouse"
        " protein, rat"
        " protein, zebrafish"
        " protein, C elegans"
        " protein, S cerevisiae"
        " protein, Drosophila"
        " protein, Arabidopsis"
        " protein, E coli"
        " protein, S pombe"
        " protein, Xenopus"
        " (mouse)"
        " (rat)"
        " (zebrafish)"
        " (C elegans)"
        " (S cerevisiae)"
        " (Drosophila)"
        " (Arabidopsis)"
        " (E coli)"
        " (S pombe)"
        " (Xenopus)"))

;; TODO -- generalize to handle predicates better (really this is more
;; of a KG standardization issue)
;;
;; (define equivalent_to (find-predicates (list "equivalent_to")))
;; (define xref (find-predicates (list "xref")))
;; (define subclass_of (find-predicates '("subclass_of")))

(define extract-name/curie/category-from-concept-ls
  (lambda (query-ls els)
    (cond
      ((null? query-ls) els)
      ((or (void? (car query-ls))
           (boolean? (car query-ls)))
       (extract-name/curie/category-from-concept-ls
        (cdr query-ls) els))
      (else 
       (match (car query-ls)
         [`(,db ,cui ,id ,name ,category ,properties-list)
          (extract-name/curie/category-from-concept-ls
           (cdr query-ls)
           (cons
            (list db id name) els))])))))

(define extract-concept-from-concept-ls
  (lambda (query-ls els curie kg)
    (cond
      ((null? query-ls) (remove-duplicates els))
      ((or (void? (car query-ls))
           (boolean? (car query-ls)))
       (extract-concept-from-concept-ls
        (cdr query-ls) els curie kg))
      (else 
       (match (car query-ls)
         [`(,db ,cui ,id ,name ,category ,properties-list)
          (cond
            ((and (equal? db kg)
                  (string-contains? id curie)) 
             (extract-concept-from-concept-ls
              (cdr query-ls)
              (set-union 
               `(,db ,cui ,id ,name ,category ,properties-list) els) curie kg))
            (else
             (extract-concept-from-concept-ls
                         (cdr query-ls)
                         els curie kg)))])))))

(define extract-curie-from-concept-ls
  (lambda (query-ls els curie kg)
    (cond
      ((null? query-ls) (flatten (remove-duplicates els)))
      ((or (void? (car query-ls))
           (boolean? (car query-ls)))
       (extract-curie-from-concept-ls
        (cdr query-ls) els curie kg))
      (else 
       (match (car query-ls)
         [`(,db ,cui ,id ,name ,category ,properties-list)
          (cond
            ((and (equal? db kg)
                  (string-contains? id curie)) 
             (extract-curie-from-concept-ls
              (cdr query-ls)
              (cons
               id els) curie kg))
            (else
             (extract-curie-from-concept-ls
                         (cdr query-ls)
                         els curie kg)))])))))

(define extract-name-from-concept-ls
  (lambda (query-ls els curie kg)
    (cond
      ((null? query-ls) (remove-duplicates els))
      ((or (void? (car query-ls))
           (boolean? (car query-ls)))
       (extract-name-from-concept-ls
        (cdr query-ls) els curie kg))
      (else 
       (match (car query-ls)
         [`(,db ,cui ,id ,name ,category ,properties-list)
          (cond
            ((and (equal? db kg)
                  (string-contains? id curie)) 
             (extract-name-from-concept-ls
              (cdr query-ls)
              (cons
               name els) curie kg))
            (else
             (extract-name-from-concept-ls
                         (cdr query-ls)
                         els curie kg)))])))))




(define HGNC-CURIE->synonymized-concepts
  (lambda (hgnc-curie)
    (define HGNC-gene-query (find-concepts #t (list hgnc-curie)))

    (when DEBUG-SYNONYMIZE
      (newline)
      (displayln (format "CONCEPTS FOUND RETURNED FROM INITIAL INPUT CURIE: ~a" HGNC-gene-query))
      (newline)
      (pretty-print (extract-name/curie/category-from-concept-ls HGNC-gene-query '())))
    
    (match-define
      (list A-->HGNC-input-->B=>concepts
            A-->HGNC-input-->B=>edges)
      (run/graph
        ((A #f)
         (HGNC-input HGNC-gene-query)
         (B #f))
        ((--equivalent_to--> '((rtx2 57 . "equivalent_to")))
         (--xref--> '((rtx2 3 . "xref"))))
        (A --equivalent_to--> HGNC-input --xref--> B)))

    (define A-->HGNC-input/concepts
      (hash-ref A-->HGNC-input-->B=>concepts 'A))

    (define HGNC-input-->B/concepts
      (hash-ref A-->HGNC-input-->B=>concepts 'B))

    (when DEBUG-SYNONYMIZE
      (newline)
      (displayln "CONCEPTS FROM:\nA --equivalent_to--> HGNC-input")
      (newline)
      (pretty-print (extract-name/curie/category-from-concept-ls A-->HGNC-input/concepts '()))
      (newline)

      (newline)
      (displayln "CONCEPTS FROM:\nHGNC-input --xref--> B")
      (newline)
      (pretty-print (extract-name/curie/category-from-concept-ls HGNC-input-->B/concepts '()))
      (newline))

    (define HGNC-NCBIGene-ENSEMBL-CUIg-OMIM/concept-ls
      (set-union
       HGNC-gene-query
       (set-union
        A-->HGNC-input/concepts
        HGNC-input-->B/concepts)))

    (when DEBUG-SYNONYMIZE
      (newline)
      (displayln "CONCEPTS FROM:\nA --equivalent_to--> HGNC-input --xref--> B")
      (newline)
      (pretty-print (extract-name/curie/category-from-concept-ls HGNC-NCBIGene-ENSEMBL-CUIg-OMIM/concept-ls '()))
      (newline))

    ;; get NCITg (if it exists), OMIM, and redundant HGNC
    (match-define
      (list A-->CUIg=>concepts
            A-->CUIg=>edges)
      (run/graph
        ((A #f)
         (CUIg HGNC-input-->B/concepts))
        ((--xref--> '((rtx2 3 . "xref"))))
        (A --xref--> CUIg)))

    (define A-->CUIg/concept-ls
      (hash-ref A-->CUIg=>concepts 'A))

    (when DEBUG-SYNONYMIZE
      ;; NCITg (if it exists), OMIM, and redundant HGNC
      (newline)
      (displayln "CONCEPTS FROM:\nA --xref--> CUI gene")
      (newline)
      (pretty-print (extract-name/curie/category-from-concept-ls A-->CUIg/concept-ls '()))
      (newline))
    
    (define HGNC-NCBIGene-ENSEMBL-CUIg-NCITg/concept-ls
      (set-union
       HGNC-NCBIGene-ENSEMBL-CUIg-OMIM/concept-ls
       A-->CUIg/concept-ls))


    ;; TODO, filter specifically on the NCITg concept if it exists!! 
    (define NCITg-concept-ls
      A-->CUIg/concept-ls)

    

    ;;get NCIT wt Allele
    (match-define
      (list A-->NCITg=>concepts
            A-->NCITg=>edges)
      (run/graph
        ((A #f)
         (NCITg A-->CUIg/concept-ls))
        ((--subclass_of--> '((rtx2 15 . "subclass_of"))))
        (A --subclass_of--> NCITg)))

    (define A-->NCITg/concept-ls
      (hash-ref A-->NCITg=>concepts 'A))

    (when DEBUG-SYNONYMIZE
      ;;should have NCIT wt Allele
      (newline)
      (displayln "CONCEPTS FROM:\nA --subclass_of--> NCIT gene:")
      (newline)
      (pretty-print (extract-name/curie/category-from-concept-ls A-->NCITg/concept-ls '()))
      (newline))

    (define HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt/concept-ls
      (set-union
       A-->NCITg/concept-ls
       HGNC-NCBIGene-ENSEMBL-CUIg-NCITg/concept-ls))

    (when DEBUG-SYNONYMIZE
      (newline)
      (displayln "CONCEPTS FROM:\nA --equivalent_to--> HGNC-input --xref--> B\nA --xref--> CUI gene\nA --subclass_of--> NCIT gene\n")
      (pretty-print (extract-name/curie/category-from-concept-ls HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt/concept-ls '()))
      (newline))

    (match-define
      (list NCITwt-->A=>concepts
            NCITwt-->A=>edges)
      (run/graph
        ((A #f)
         (NCITwt A-->NCITg/concept-ls))
        ((--xref--> '((rtx2 3 . "xref"))))
        (NCITwt --xref--> A)))

    (define NCITwt-->A/concept-ls
      (hash-ref NCITwt-->A=>concepts 'A))

    (when DEBUG-SYNONYMIZE
      ;; should have CUI wt Allele
      (newline)
      (displayln "CONCEPTS FROM:\nNCIT wt Allele --xref--> A")
      (pretty-print (extract-name/curie/category-from-concept-ls NCITwt-->A/concept-ls '()))
      (newline))

    (define HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt/concept-ls
      (set-union
       NCITwt-->A/concept-ls
       HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt/concept-ls))

    (when DEBUG-SYNONYMIZE
      (newline)
      (displayln "CONCEPTS FROM:\nA --equivalent_to--> HGNC-input --xref--> B\nA --xref--> CUI gene\nA --subclass_of--> NCIT gene\nNCIT wt Allele --xref--> A\n" )
      (newline)
      (pretty-print (extract-name/curie/category-from-concept-ls HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt/concept-ls '()))
      (newline))

    (define NCBIGene-concept/rtx2
      (extract-concept-from-concept-ls
       HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt/concept-ls '() "NCBIGene:" 'rtx2))

    (when DEBUG-SYNONYMIZE
      (newline)
      (displayln "NCBIGene-concept/rtx2:")
      (newline)
      (pretty-print NCBIGene-concept/rtx2)
      (newline))

    (define encodes (find-predicates (list "encodes")))

    (match-define
      (list NCBI-input-->Y=>concepts
            NCBI-input-->Y=>edges)
      (run/graph
        ((Y #f)
         (NCBIg (list NCBIGene-concept/rtx2)))
        ((--encodes--> '((rtx2 775 . "encodes"))))
        (NCBIg --encodes--> Y)
        ))


    ;; returns a redundant CUI for gene as well as desired CUI for protein
    ;; has the CUIprotein concept
    (define NCBI-input-->Y/concepts
      (hash-ref NCBI-input-->Y=>concepts 'Y))

    (when DEBUG-SYNONYMIZE
      (newline)
      (displayln "CONCEPTS FROM:\nNCBIGene --encodes--> B")
      (newline)
      (pretty-print (extract-name/curie/category-from-concept-ls NCBI-input-->Y/concepts '()))
      (newline))

    (match-define
      (list A-->NCBI-input=>concepts
            A-->NCBI-input=>edges)
      (run/graph
        ((A #f)
         (NCBIg (list NCBIGene-concept/rtx2))) 
        ((--xref--> '((rtx2 3 . "xref"))))
        (A --xref--> NCBIg)))

    (define A-->NCBIGene/concepts 
      (hash-ref A-->NCBI-input=>concepts 'A))

    (when DEBUG-SYNONYMIZE
      (newline)
      (displayln "CONCEPTS FROM:\nA --xref--> NCBIGene")
      (newline)
      (pretty-print (extract-name/curie/category-from-concept-ls A-->NCBIGene/concepts '()))
      (newline))

    (define NCBIGene-concept/orange 
      (find-concepts
       #t
       (extract-curie-from-concept-ls
        HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt/concept-ls '() "NCBIGene:" 'rtx2)))


    (define HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt-CUIp-UniProtKB/concept-ls
      (set-union NCBIGene-concept/orange
                 (set-union A-->NCBIGene/concepts
                            (set-union
                             NCBI-input-->Y/concepts
                             HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt/concept-ls))))

    (when DEBUG-SYNONYMIZE
      (newline)
      (displayln "CONCEPTS FROM:\nA --equivalent_to--> HGNC-input --xref--> B\nA --xref--> CUI gene\nA --subclass_of--> NCIT gene\nNCIT wt Allele --xref--> A\nA --xref--> NCBIGene\nNCBIGene --encodes--> B\n" )
      (newline)
      (pretty-print (extract-name/curie/category-from-concept-ls HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt-CUIp-UniProtKB/concept-ls '()))
      (newline))

    (match-define
      (list A-->CUIp-input=>concepts
            A-->CUIp-input=>edges)
      (run/graph
        ((A #f)
         (CUIp A-->NCBIGene/concepts))
        ((--xref--> '((rtx2 3 . "xref"))))
        (A --xref--> CUIp)))

    ;; should have MESH protein id
    (define A-->CUIp-input/concepts
      (hash-ref A-->CUIp-input=>concepts 'A))
    
    (when DEBUG-SYNONYMIZE
      (newline)
      (displayln "CONCEPTS FROM:\nA --xref--> CUI protein")
      (newline)
      (pretty-print (extract-name/curie/category-from-concept-ls A-->CUIp-input/concepts '()))
      (newline))

    (define HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt-CUIp-UniProtKB-MESHp/concept-ls
      (set-union
       A-->CUIp-input/concepts
       HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt-CUIp-UniProtKB/concept-ls))

    (when DEBUG-SYNONYMIZE
      (newline)
      (displayln "CONCEPTS FROM:\nA --equivalent_to--> HGNC-input --xref--> B\nA --xref--> CUI gene\nA --subclass_of--> NCIT gene\nNCIT wt Allele --xref--> A\nA --xref--> NCBIGene\nNCBIGene --encodes--> B\nA --xref--> CUI protein\n")
      (newline)
      (pretty-print (extract-name/curie/category-from-concept-ls HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt-CUIp-UniProtKB-MESHp/concept-ls '()))
      (newline))

    (define molecular-entity/concept-ls/sans-UMLS
      HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt-CUIp-UniProtKB-MESHp/concept-ls) 


    (define molecular-entity/curie-ls/CUI-ONLY
      (extract-curie-from-concept-ls molecular-entity/concept-ls/sans-UMLS '() "CUI:" 'rtx2))


    (define molecular-entity/curie-ls/UMLS-ONLY
      (map (lambda (curie)
             (string-replace curie "CUI:" "UMLS:"))
           molecular-entity/curie-ls/CUI-ONLY))

    (define molecular-entity/concept-ls/UMLS-ONLY
      (find-concepts #t molecular-entity/curie-ls/UMLS-ONLY))

    ;; dangerous!  Not every HGNC in rtx2 is in robokop
    #|
    (define molecular-entity/curie-ls/HGNC-ONLY
    (extract-name-from-concept-ls molecular-entity/concept-ls/sans-UMLS '() "HGNC:" 'robokop))
    |#

    (define molecular-entity/curie-ls/HGNC-ONLY
      (list (cadr (regexp-match #px"^HGNC:([0-9]+)" hgnc-curie))))
    
    
    ;; string append HGNC symbol to list of human suffixes
    (define HGNC-string-with-human-gene/protein-suffix-ls
      (map (lambda (str) (string-append (car molecular-entity/curie-ls/HGNC-ONLY) str)) human-gene/protein-suffix-ls))

    ;; use this to filter find-concepts 
    (define HGNC-string-with-human-gene/protein-suffix-concept-ls
      (remove-duplicates
       (apply append
              (map
                (lambda (x) (find-concepts #f (list x)))
                HGNC-string-with-human-gene/protein-suffix-ls))))

    (define filtered-HGNC-string-with-human-gene/protein-suffix-concept-ls
      (filter (lambda (c)
                (member (concept->name c)
                        HGNC-string-with-human-gene/protein-suffix-ls))
              HGNC-string-with-human-gene/protein-suffix-concept-ls
              ))

    (define molecular-entity-concept-ls/complete
      (set-union filtered-HGNC-string-with-human-gene/protein-suffix-concept-ls 
                 (set-union molecular-entity/concept-ls/sans-UMLS
                            molecular-entity/concept-ls/UMLS-ONLY)))

    (define human-gene/protein-concept-ls molecular-entity-concept-ls/complete)

    (when DEBUG-SYNONYMIZE
      (newline)
      (displayln "CONCEPT BUILDING FOR HGNC QUERY COMPLETE:\n")
      (pretty-print (extract-name/curie/category-from-concept-ls human-gene/protein-concept-ls '()))
      (newline))

    (define HGNC-string-with-animal-model/bacteria/plant-gene/protein-suffix-ls
      (let ((hgnc-id-str (car molecular-entity/curie-ls/HGNC-ONLY)))
        (map string-downcase
             (map (lambda (x) (string-append hgnc-id-str x))
                  animal-model/bacteria/plant-gene/protein-suffix-ls))))

    (define HGNC-string-with-animal-model/bacteria/plant-gene/protein-suffix-concept-ls
      (remove-duplicates
       (apply append
              (map (lambda (x) (find-concepts #f (list x)))
                   HGNC-string-with-animal-model/bacteria/plant-gene/protein-suffix-ls))))

    (define filtered-HGNC-string-with-animal-model/bacteria/plant-gene/protein-suffix-concept-ls
      (filter (lambda (c)
                (member (string-downcase (concept->name c))
                        HGNC-string-with-animal-model/bacteria/plant-gene/protein-suffix-ls))
              HGNC-string-with-animal-model/bacteria/plant-gene/protein-suffix-concept-ls)) 

    (define animal-genes/proteins-concept-ls filtered-HGNC-string-with-animal-model/bacteria/plant-gene/protein-suffix-concept-ls)

    (when DEBUG-SYNONYMIZE
      (newline)
      (displayln "CONCEPT BUILDING FOR ANIMAL MODEL QUERY COMPLETE:\n")
      (pretty-print (extract-name/curie/category-from-concept-ls animal-genes/proteins-concept-ls '()))
      (newline))
    
    (define all-genes/proteins (set-union human-gene/protein-concept-ls
                                          animal-genes/proteins-concept-ls))
    
    (hash

     'all-genes/proteins
     all-genes/proteins

     'human-genes/proteins
     human-gene/protein-concept-ls

     'animal-genes/proteins
     animal-genes/proteins-concept-ls
     )
    ))



;; RHOBTB2
;; (define RHOBTB2-synonyms (HGNC-CURIE->synonymized-concepts "HGNC:18756"))

;; E2F1
;; (define E2F1-synonyms (HGNC-CURIE->synonymized-concepts "HGNC:3113"))
