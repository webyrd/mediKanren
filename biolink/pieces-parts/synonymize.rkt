#lang racket
(provide (all-defined-out)
         (all-from-out "../common.rkt" "../mk-db.rkt"))
(require "../common.rkt" "../mk-db.rkt")




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


(define genetic_variant-curie-ls
  '("HGNC:18756"))

(define HGNC-gene-query
  (find-concepts #t genetic_variant-curie-ls))


(newline)
(displayln (format "CONCEPTS FOUND RETURNED FROM INITIAL INPUT CURIE: ~a" genetic_variant-curie-ls))
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls HGNC-gene-query '()))
          

(define equivalent_to (find-predicates (list "equivalent_to")))
(define xref (find-predicates (list "xref")))

(match-define
 (list A-->HGNC-input-->B=>concepts
       A-->HGNC-input-->B=>edges)
 (time
  (run/graph
   ((A #f)
    (HGNC-input HGNC-gene-query)
    (B #f))
   ((--equivalent_to--> '((rtx2 57 . "equivalent_to")))
    (--xref--> '((rtx2 3 . "xref"))))
   (A --equivalent_to--> HGNC-input --xref--> B))))

(define A-->HGNC-input/concepts
  (hash-ref A-->HGNC-input-->B=>concepts 'A))

(define HGNC-input-->B/concepts
  (hash-ref A-->HGNC-input-->B=>concepts 'B))


(newline)
(displayln "CONCEPTS FROM:\nA --equivalent_to--> HGNC-input")
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls A-->HGNC-input/concepts '()))
(newline)

(newline)
(displayln "CONCEPTS FROM:\nHGNC-input --xref--> B")
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls HGNC-input-->B/concepts '()))
(newline)


(define HGNC-NCBIGene-ENSEMBL-CUIg-OMIM/concept-ls
  (set-union
   HGNC-gene-query
   (set-union
    A-->HGNC-input/concepts
    HGNC-input-->B/concepts)))


(newline)
(displayln "CONCEPTS FROM:\nA --equivalent_to--> HGNC-input --xref--> B")
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls HGNC-NCBIGene-ENSEMBL-CUIg-OMIM/concept-ls '()))
(newline)


;; get NCITg (if it exists), OMIM, and redundant HGNC
(match-define
 (list A-->CUIg=>concepts
       A-->CUIg=>edges)
 (time
  (run/graph
   ((A #f)
    (CUIg HGNC-input-->B/concepts))
   ((--xref--> '((rtx2 3 . "xref"))))
   (A --xref--> CUIg))))

(define A-->CUIg/concept-ls
  (hash-ref A-->CUIg=>concepts 'A))


;; NCITg (if it exists), OMIM, and redundant HGNC
(newline)
(displayln "CONCEPTS FROM:\nA --xref--> CUI gene")
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls A-->CUIg/concept-ls '()))
(newline)

(define HGNC-NCBIGene-ENSEMBL-CUIg-NCITg/concept-ls
  (set-union
   HGNC-NCBIGene-ENSEMBL-CUIg-OMIM/concept-ls
   A-->CUIg/concept-ls))

(define NCITg-concept-ls
  A-->CUIg/concept-ls)

(define subclass_of (find-predicates '("subclass_of")))

;;get NCIT wt Allele
(match-define
  (list A-->NCITg=>concepts
        A-->NCITg=>edges)
  (time
   (run/graph
    ((A #f)
     (NCITg A-->CUIg/concept-ls))
    ((--subclass_of--> '((rtx2 15 . "subclass_of"))))
    (A --subclass_of--> NCITg))))

(define A-->NCITg/concept-ls
  (hash-ref A-->NCITg=>concepts 'A))


;;should have NCIT wt Allele 
(newline)
(displayln "CONCEPTS FROM:\nA --subclass_of--> NCIT gene:")
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls A-->NCITg/concept-ls '()))
(newline)


(define HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt/concept-ls
  (set-union
   A-->NCITg/concept-ls
   HGNC-NCBIGene-ENSEMBL-CUIg-NCITg/concept-ls))


(newline)
(displayln "CONCEPTS FROM:\nA --equivalent_to--> HGNC-input --xref--> B\nA --xref--> CUI gene\nA --subclass_of--> NCIT gene\n")
(pretty-print (extract-name/curie/category-from-concept-ls HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt/concept-ls '()))
(newline)



(match-define
  (list NCITwt-->A=>concepts
        NCITwt-->A=>edges)
  (time
   (run/graph
    ((A #f)
     (NCITwt A-->NCITg/concept-ls))
    ((--xref--> '((rtx2 3 . "xref"))))
    (NCITwt --xref--> A))))

(define NCITwt-->A/concept-ls
  (hash-ref NCITwt-->A=>concepts 'A))


;; should have CUI wt Allele
(newline)
(displayln "CONCEPTS FROM:\nNCIT wt Allele --xref--> A")
(pretty-print (extract-name/curie/category-from-concept-ls NCITwt-->A/concept-ls '()))
(newline)


(define HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt/concept-ls
  (set-union
   NCITwt-->A/concept-ls
   HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt/concept-ls))



(newline)
(displayln "CONCEPTS FROM:\nA --equivalent_to--> HGNC-input --xref--> B\nA --xref--> CUI gene\nA --subclass_of--> NCIT gene\nNCIT wt Allele --xref--> A\n" )
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt/concept-ls '()))
(newline)


(define NCBIGene-concept
  (extract-concept-from-concept-ls
   HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt/concept-ls '() "NCBIGene:" 'rtx2))




(newline)
(displayln "NCBIGene-concept:")
(newline)
(pretty-print NCBIGene-concept)
(newline)


(define encodes (find-predicates (list "encodes")))

(match-define
  (list NCBI-input-->Y=>concepts
        NCBI-input-->Y=>edges)
  (time
   (run/graph
    ((Y #f)
     (NCBIg (list NCBIGene-concept)))
    ((--encodes--> '((rtx2 775 . "encodes"))))
    (NCBIg --encodes--> Y)
    )))


;; returns a redundant CUI for gene as well as desired CUI for protein
;; has the CUIprotein concept
(define NCBI-input-->Y/concepts
  (hash-ref NCBI-input-->Y=>concepts 'Y))


(newline)
(displayln "CONCEPTS FROM:\nNCBIGene --encodes--> B")
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls NCBI-input-->Y/concepts '()))
(newline)


(match-define
  (list A-->NCBI-input=>concepts
        A-->NCBI-input=>edges)
  (time
   (run/graph
    ((A #f)
     (NCBIg (list NCBIGene-concept))) 
    ((--xref--> '((rtx2 3 . "xref"))))
    (A --xref--> NCBIg))))

(define A-->NCBIGene/concepts 
  (hash-ref A-->NCBI-input=>concepts 'A))

(newline)
(displayln "CONCEPTS FROM:\nA --xref--> NCBIGene")
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls A-->NCBIGene/concepts '()))
(newline)

(define HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt-CUIp-UniProtKB/concept-ls
  (set-union A-->NCBIGene/concepts
             (set-union
              NCBI-input-->Y/concepts
              HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt/concept-ls)))


(newline)
(displayln "CONCEPTS FROM:\nA --equivalent_to--> HGNC-input --xref--> B\nA --xref--> CUI gene\nA --subclass_of--> NCIT gene\nNCIT wt Allele --xref--> A\nA --xref--> NCBIGene\nNCBIGene --encodes--> B\n" )
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt-CUIp-UniProtKB/concept-ls '()))
(newline)


(match-define
  (list A-->CUIp-input=>concepts
        A-->CUIp-input=>edges)
  (time
   (run/graph
    ((A #f)
     (CUIp A-->NCBIGene/concepts))
    ((--xref--> '((rtx2 3 . "xref"))))
    (A --xref--> CUIp))))

;; should have MESH protein id
(define A-->CUIp-input/concepts
  (hash-ref A-->CUIp-input=>concepts 'A))


(newline)
(displayln "CONCEPTS FROM:\nA --xref--> CUI protein")
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls A-->CUIp-input/concepts '()))
(newline)


(define HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt-CUIp-UniProtKB-MESHp/concept-ls
  (set-union
   A-->CUIp-input/concepts
   HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt-CUIp-UniProtKB/concept-ls))


(newline)
(displayln "CONCEPTS FROM:\nA --equivalent_to--> HGNC-input --xref--> B\nA --xref--> CUI gene\nA --subclass_of--> NCIT gene\nNCIT wt Allele --xref--> A\nA --xref--> NCBIGene\nNCBIGene --encodes--> B\nA --xref--> CUI protein\n")
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt-CUIp-UniProtKB-MESHp/concept-ls '()))
(newline)


(define molecular-entity/concept-ls/sans-UMLS
  HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt-CUIp-UniProtKB-MESHp/concept-ls) 


(define molecular-entity/curie-ls/CUI-ONLY
  (extract-curie-from-concept-ls molecular-entity/concept-ls/sans-UMLS '() "CUI:" 'rtx2))

(define molecular-entity/curie-ls/UMLS-ONLY
  (map (lambda (ls)
         (string-replace
          (car (list ls))
          "CUI:" "UMLS:")) molecular-entity/curie-ls/CUI-ONLY))

(define molecular-entity/concept-ls/UMLS-ONLY
  (find-concepts #t molecular-entity/curie-ls/UMLS-ONLY))


(define molecular-entity-concept-ls/complete
  (set-union molecular-entity/concept-ls/sans-UMLS
             molecular-entity/concept-ls/UMLS-ONLY))

(newline)
(displayln "CONCEPT BUILDING FOR HGNC QUERY COMPLETE:\n")
(pretty-print (extract-name/curie/category-from-concept-ls molecular-entity-concept-ls/complete '()))
(newline)


;;CUI to UMLS mappings are not perfect, CUI:C1418837 did not map to UMLS
;;not all CUI proteins have a xref to MESH protein
;; MESH:C517191 is PPP2RA1 protein, human. it has xref to CUI:C1871283 PPP2R1A protein, human
;; but there is no way to get at that CUI
;; MESH:C517191 --mapped_to--> MESH:D054648
;; MESH:C517191 --xref--> CUI:C1871283
;; MESH:D appears to master node for all species of protein
;; should I string append protein, human protein, rat protein, mouse

molecular-entity-concept-ls/complete

(match-define
  (list A-->molecular-entity-concept-ls/complete-->B=>concepts
        A-->molecular-entity-concept-ls/complete-->B=>edges)
  (time
   (run/graph
    ((A #f)
     (mol-entity-ls molecular-entity-concept-ls/complete)
     (B #f))
    ((--ALLin--> #f)
     (--ALLout--> #f))
    (A --ALLin--> mol-entity-ls --ALLout--> B))))


(define A-->mol-entity-ls/concepts
  (hash-ref A-->molecular-entity-concept-ls/complete-->B=>concepts 'A))

(define mol-entity-ls-->B/concepts
  (hash-ref A-->molecular-entity-concept-ls/complete-->B=>concepts 'B))

(define A--ALL-->mol-entity-ls/edges
  (hash-ref A-->molecular-entity-concept-ls/complete-->B=>edges '--ALLin-->))
#|
(displayln "A--ALL-->mol-entity-ls/edges")
(newline)
(pretty-print A--ALL-->mol-entity-ls/edges)
(newline)
|#
(define mol-entity-ls--ALL-->B/edges
  (hash-ref A-->molecular-entity-concept-ls/complete-->B=>edges '--ALLout-->))
#|
(displayln "mol-entity-ls--ALL-->B/edges")
(newline)
(pretty-print mol-entity-ls--ALL-->B/edges)
(newline)
|#
