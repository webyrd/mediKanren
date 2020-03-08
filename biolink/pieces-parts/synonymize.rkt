#lang racket
(provide (all-defined-out)
         (all-from-out "../common.rkt" "../mk-db.rkt"))
(require "../common.rkt" "../mk-db.rkt")



;; just for printing
(define extract-name/curie/category-from-concept-ls
  (lambda (concept)
    (list (concept->name concept)
          (concept->curie concept)
          (concept->category concept))))

(define genetic_variant-curie-ls
  (list "HGNC:18756"))

(define HGNC-gene-query
  (find-concepts #t genetic_variant-curie-ls))


(newline)
(displayln (format "CONCEPTS FOUND RETURNED FROM INITIAL INPUT CURIE: ~a" genetic_variant-curie-ls))
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls HGNC-gene-query))
          

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
(pretty-print (extract-name/curie/category-from-concept-ls A-->HGNC-input/concepts))
(newline)

(newline)
(displayln "CONCEPTS FROM:\nHGNC-input --xref--> B")
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls HGNC-input-->B/concepts))
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
(pretty-print (extract-name/curie/category-from-concept-ls HGNC-NCBIGene-ENSEMBL-CUIg-OMIM/concept-ls))
(newline)


;; get NCITg
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

          
;; should have NCIT-gene
(newline)
(displayln "CONCEPTS FROM:\nA --xref--> CUI gene")
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls A-->CUIg/concept-ls))
(newline)



(define HGNC-NCBIGene-ENSEMBL-CUIg-NCITg/concept-ls
  (set-union
   HGNC-NCBIGene-ENSEMBL-CUIg-OMIM/concept-ls
   A-->CUIg/concept-ls))

(define NCITg-concept-ls
  A-->CUIg/concept-ls)

(define subclass_of (find-predicates '("subclass_of")))
