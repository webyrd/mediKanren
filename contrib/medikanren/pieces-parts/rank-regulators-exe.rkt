#lang racket
(require "../../../medikanren/pieces-parts/rank-regulators.rkt"
         "../../../medikanren/pieces-parts/rank-regulators-gene-lists.rkt")
#|
Purpose: The purpose of this program is to find genes that have the most influence over a user inputted
         list of genes of interest.

Methods: It does this by using two separate methods of querying the knowledge graphs: (1) querying the GO
         ontology and (2) using a 1-hop query. The relationships found through the GO query are looser 
         because the indirect, multi-hop query requires that we make bigger inferences. However, we have
         more confidence in the information in GO. The 1-hop query looks for more direct relationships
         and interactions between genes and proteins, but the information that that query returns could
         be inaccurate due to inaccuracies in NLP generated knowledge graphs. After using both querying
         methods, we compare the two results to find intersections between them. The results that are 
         common between both methods are held in higher confidence, but that is not to say that results
         that don't lie in the overlap are inaccurate.

Input/Output: Takes a list of the curies of genes of interest and outputs a sorted association list of
              gene to regulated gene relationships. The association list is sorted from most to least
              by the number of other genes/gene products that a gene regulates.

Information in Output: Contained within the sorted association list that is returned is a pair made from
                       cons-ing the regulator gene's (G) curie and name. These (G curie. G name) pairs are
                       the keys of the assoc list. The value is a list of lists that contains the regulated
                       genes (rG) that the regulator gene (G) regulates along with additional information.
                       Each element in the value is a list that contains the rG's curie, name, the presumed
                       direction of regulation (told by predicates), and provenance for the edges in the form
                       of pubmed ids. The functions 1-hop-query/report and find-intersections also include
                       a boolean that indicates whether or not the G and rG directly interact with each other
                       in the body (#t if yes) and provenance for that information also in the form of pubmed
                       ids.

Note: The functions called in this file are more extensively documented in rank-regulators.rkt
|#

#|
The following calls are an example based off an ARDS gene list, defined as ards-genes. There are 517 ENSEMBL
gene curies on that list, and they yield 468 uniprot curies.

Note: runtimes given in comments below are based off a 2020 MacBook Pro (2 GHz Quad-Core Intel Core i5,
      16G RAM). They are meant to give an idea of which calls take a lot longer to finish.
|#

#|
Turns the inputted gene list of interest into a list of uniprot curies in order to be passed
into the GO query.
runtime: < 30s
|#
;;function call: genes->uniprots | takes: list of gene curies | returns: list of uniprot curies
(define uniprots (time (genes->uniprots ards-genes)))

#|
The next two statements 1) run the GO query and 2) reports the results of the query in an association list.
runtime:
    GO-query-edges: ~5 min
    go-query-results: just under 2 hrs
|#
;;function call: query-GO | takes: list of uniprot curies | returns: list of three edge hashes
(define GO-query-edges (time (query-GO uniprots)))

;;function call: report-GO-queries | takes: list of three edge hashes | returns: sorted assoc list
(define go-query-results (time (report-GO-queries GO-query-edges)))

#|
Runs and reports the 1-hop query.
runtime: ~ 20 min
|#
;;function call: 1-hop-query/report | takes: list of uniprot curies | returns: sorted assoc list
(define 1-hop-results (time (1-hop-query/report uniprots)))

#|
Finds the intersections between the GO query findings and 1-hop query findings.
|#
;;function call: find-intersections | takes: sorted assoc list returned by report-GO-queries, sorted
;;assoc list returned by 1-hop-query/report | returns: sorted assoc list
;; NOTE: CURRENTLY THROWS ERROR
;;(define intersections (time (find-intersections go-query-results 1-hop-results)))

;;function call: find-expression-locations | takes: list of gene/protein curies | returns: hash that maps cell/tissue type to list of gene/proteins
;;runtime: ~25 min
(define cell/tissue-types (time (find-expression-locations (set-union (map caar go-query-results) (map caar 1-hop-results) uniprots))))

;;function call: sort-cell/tissue-types-by-G-and-rG | takes: hash returned by find-expression-locations | returns: sorted assoc list
(define cell/tissue-types-sorted-by-G-or-rG (time (sort-cell/tissue-types-by-G-and-rG cell/tissue-types)))

;;function call: sort-cell/tissue-types-by-rG | takes: hash returned by find-expression-locations | returns: sorted assoc list
(define cell/tissue-types-sorted-by-rG (time (sort-cell/tissue-types-by-rG cell/tissue-types (map curie->curie/name uniprots))))

#|
Gives a ranked list of G's with the count of the number of relationships they have with an rG. The same rG
can be counted twice if the nature of the relationship between the G and rG (ie. the direction or direct
interaction) is different.
First is for go query results, second is for 1-hop-results
|#
(define go-ranked-Gs-indistinct (map (lambda (x) (cons (car x) (length (cdr x)))) go-query-results))
(define 1-hop-ranked-Gs-indistinct (map (lambda (x) (cons (car x) (length (cdr x)))) 1-hop-results))

#|
Gives a ranked list of G's with the count of the number of distinct rG's they regulate. The same rG cannot be
counted twice.
First is for go query results, second is for 1-hop-results
|#
(define go-ranked-Gs-distinct (sort
                               (map
                                (lambda (g=>rg)
                                  (cons (car g=>rg)
                                        (length (group-by (lambda (rg/info) (caar rg/info)) (cdr g=>rg))))
                                  )
                                go-query-results
                                )
                               >
                               #:key (lambda (x) (cdr x))))
(define 1-hop-ranked-Gs-distinct (sort
                                  (map
                                   (lambda (g=>rg)
                                     (cons (car g=>rg)
                                           (length (group-by (lambda (rg/info) (caar rg/info)) (cdr g=>rg))))
                                     )
                                   1-hop-results
                                   )
                                  >
                                  #:key (lambda (x) (cdr x))))

#|
Gives the top 10 ranked G's that are also on ARDS gene list
|#
(define go-top-10-Gs-on-ards-list (map curie->curie/name (take (set-intersect (map caar go-ranked-Gs-distinct) uniprots) 10)))