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

(define substitute
  (lambda (ls old new)
    (cond 
      ((null? ls) '())
      ((void? (car ls))
       (substitute (cdr ls) old new))
      ((boolean? (car ls))
       (cons
         (format "~a" (car ls))
         (substitute (cdr ls) old new))) 
      ((equal? (car ls) old)
       (cons new
             (substitute (cdr ls) old new)))
      (else
       (cons (car ls)
             (substitute (cdr ls) old new))))))

(define str-converter
  (lambda (ls)
    (cond
      ((null? ls)
       (substitute ls '() "NA"))
      ((or (boolean? (car ls))
           (void? (car ls)))
       (str-converter (cdr ls)))
      (else 
       (if (symbol? (car ls))
           (string-join (map symbol->string ls) " ")
           (string-join ls " "))))))

(define PUBMED_URL_PREFIX "https://www.ncbi.nlm.nih.gov/pubmed/")

(define export-column-headers
  (lambda (headers port path)
    (cond 
      ((= (file-size path) 0)
       (cond
         ((null? headers)
          (fprintf port "~c" #\newline))
         (else
          (fprintf port "~a~c" (car headers) #\tab)
          (export-column-headers (cdr headers) port path))))
      (else
       (void)))))

(define outer-loop
  (lambda (ls port)
    (cond
      ((null? ls)
       (close-output-port port))
      (else
       (inner-loop (car ls) port)
       (fprintf port (format "~c" #\newline))
       (outer-loop (cdr ls) port)))))

(define inner-loop
  (lambda (ls port)
    (cond
      ((null? ls) (void))
      (else
       (fprintf port "~a~c" (car ls) #\tab)
       (inner-loop (cdr ls) port)))))

(define equivalent_to (find-predicates (list "equivalent_to")))
(define xref (find-predicates (list "xref")))



 
(define test
  "test")

(define genetic_variant-curie-ls
  '("HGNC:18756"))

(define HGNC-gene-query
  (find-concepts #t genetic_variant-curie-ls))

(newline)
(displayln (format "CONCEPTS FOUND RETURNED FROM INITIAL INPUT CURIE: ~a" HGNC-gene-query))
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls HGNC-gene-query '()))
         

#|	       
(define export-path
  (cdr (assoc "automated_query_export_path" (config))))               

(define date (seconds->date (current-seconds)))

(define export-date 
  (format "~a_~a_~a" 
          (number->string (date-month date))
          (number->string (date-day date))
          (number->string (date-year date))))

#|CREATE EXPORT DIRECTORY|#
(define directory/path
  (format "~a~a_~a_~a/" export-path export-date test test))
          
;;read in existing file, delete it and re-export         
(define make-export-directory
  (if (directory-exists? directory/path)
      (error (format "CHECK FILE, IT MAY EXIST"))
      (make-directory directory/path)))
|#


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


;; TODO, filter specifically on the NCITg concept if it exists!! 
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


(define NCBIGene-concept/rtx2
  (extract-concept-from-concept-ls
   HGNC-NCBIGene-ENSEMBL-CUIg-NCITg-NCITwt-CUIwt/concept-ls '() "NCBIGene:" 'rtx2))


(newline)
(displayln "NCBIGene-concept/rtx2:")
(newline)
(pretty-print NCBIGene-concept/rtx2)
(newline)


(define encodes (find-predicates (list "encodes")))

(match-define
  (list NCBI-input-->Y=>concepts
        NCBI-input-->Y=>edges)
  (time
   (run/graph
    ((Y #f)
     (NCBIg (list NCBIGene-concept/rtx2)))
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
     (NCBIg (list NCBIGene-concept/rtx2))) 
    ((--xref--> '((rtx2 3 . "xref"))))
    (A --xref--> NCBIg))))

(define A-->NCBIGene/concepts 
  (hash-ref A-->NCBI-input=>concepts 'A))

(newline)
(displayln "CONCEPTS FROM:\nA --xref--> NCBIGene")
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls A-->NCBIGene/concepts '()))
(newline)


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

(define molecular-entity/curie-ls/HGNC-ONLY
  (extract-name-from-concept-ls molecular-entity/concept-ls/sans-UMLS '() "HGNC:" 'robokop))

(define human-gene/protein-suffix-ls
  (list " protein, human"
        " gene"
        " Gene"
        " wt Allele"
        " (human)"))

(define map-it
  (lambda (f ls)
    (cond
      ((null? ls) '())
      (else
       (cons (f (car ls))
             (map-it f (cdr ls)))))))

;; string append HGNC symbol to list of human suffixes
(define HGNC-string-with-human-gene/protein-suffix-ls
  (map (lambda (x) (string-append (car molecular-entity/curie-ls/HGNC-ONLY) x)) human-gene/protein-suffix-ls))


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
 
(newline)
(displayln "CONCEPT BUILDING FOR HGNC QUERY COMPLETE:\n")
(pretty-print (extract-name/curie/category-from-concept-ls molecular-entity-concept-ls/complete '()))
(newline)


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

(define HGNC-string-with-animal-model/bacteria/plant-gene/protein-suffix-ls
  (map
   string-downcase
   (map
    (lambda (x) (string-append (car molecular-entity/curie-ls/HGNC-ONLY) x))
    animal-model/bacteria/plant-gene/protein-suffix-ls)))

(define HGNC-string-with-animal-model/bacteria/plant-gene/protein-suffix-concept-ls
  (remove-duplicates
   (apply
    append
    (map
     (lambda (x) (find-concepts #f (list x)))
     HGNC-string-with-animal-model/bacteria/plant-gene/protein-suffix-ls))))

(define filtered-HGNC-string-with-animal-model/bacteria/plant-gene/protein-suffix-concept-ls
  (filter
   (lambda (c)
     (member (string-downcase (concept->name c))
             HGNC-string-with-animal-model/bacteria/plant-gene/protein-suffix-ls))
   HGNC-string-with-animal-model/bacteria/plant-gene/protein-suffix-concept-ls)) 

(newline)
(displayln "CONCEPT BUILDING FOR ANIMAL MODEL QUERY COMPLETE:\n")
(pretty-print (extract-name/curie/category-from-concept-ls filtered-HGNC-string-with-animal-model/bacteria/plant-gene/protein-suffix-concept-ls '()))
(newline)

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

(define match-edge-simple/for-export
  (lambda (edges-ls els record-id)
    (cond
      ((null? edges-ls) els)
      (else 
       (match (car edges-ls)
         [`(,db ,edge-cui (,subject-cui ,subject-id ,subject-name (,_ . ,subject-category) ,subject-props-assoc)
                (,object-cui ,object-id ,object-name (,_ . ,object-category) ,object-props-assoc)
                (,_ . ,pred) ,pred-props-assoc)
          (match-edge-simple/for-export
           (cdr edges-ls)
           (cons 
            (substitute
             (list
              record-id
              db
              subject-name
              subject-id
              subject-category
              pred
              object-name
              object-id
              object-category
              (length (pubmed-ids-from-edge-props pred-props-assoc))
              (string-join
               (map (lambda (pubmed)
                      (string-append PUBMED_URL_PREFIX (~a pubmed)))
                    (pubmed-ids-from-edge-props pred-props-assoc)) " ")) '() "NA") els) record-id)])))))

(define TARGET-CONCEPT-->ALLp-->ALLc/export-edges
  (match-edge-simple/for-export mol-entity-ls--ALL-->B/edges '() test))

(define ALLc-->ALLp-->TARGET-CONCEPT/export-edges
  (match-edge-simple/for-export A--ALL-->mol-entity-ls/edges '() test))

#|
(define COLUMN-HEADERS_SUBJECT->PREDICATE->OBJECT
  '("record_id" "knowledge_graph" "subject_name" "subject_id" "subject_category"
    "predicate" "target_object_name" "target_object_id" "target_object_category" "number_of_pubmeds"
    "pubmed_urls"))
                    
(define ALLc-->ALLp-->TARGET-CONCEPT/path
  (format			
   "~a~a_[ALLc]-->ALLp-->~a.tsv"
   directory/path test (concept->name (car HGNC-gene-query))))

(define ALLc-->ALLp-->TARGET-CONCEPT/port
  (open-output-file ALLc-->ALLp-->TARGET-CONCEPT/path #:exists 'can-update))

(export-column-headers
 COLUMN-HEADERS_SUBJECT->PREDICATE->OBJECT
 ALLc-->ALLp-->TARGET-CONCEPT/port
 ALLc-->ALLp-->TARGET-CONCEPT/path)

(outer-loop
 ALLc-->ALLp-->TARGET-CONCEPT/export-edges
 ALLc-->ALLp-->TARGET-CONCEPT/port)

(define TARGET-CONCEPT-->ALLp-->ALLc/path
  (format			
   "~a~a_~a-->ALLp-->[ALLc].tsv"
   directory/path test (car (extract-name/curie/category-from-concept-ls HGNC-gene-query))))

(define TARGET-CONCEPT-->ALLp-->ALLc/port
  (open-output-file TARGET-CONCEPT-->ALLp-->ALLc/path #:exists 'can-update))

(export-column-headers
 COLUMN-HEADERS_SUBJECT->PREDICATE->OBJECT
 TARGET-CONCEPT-->ALLp-->ALLc/port
 TARGET-CONCEPT-->ALLp-->ALLc/path)

(outer-loop
 TARGET-CONCEPT-->ALLp-->ALLc/export-edges
 TARGET-CONCEPT-->ALLp-->ALLc/port)
 
|#
