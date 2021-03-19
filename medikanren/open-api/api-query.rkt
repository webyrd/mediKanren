#lang racket
(provide (all-defined-out))
(require "../mk.rkt" json net/url)

(define (js-count js)
  (cond ((pair? js) (foldl + 0 (map js-count js)))
        ((hash? js) (foldl + 0 (hash-map js (lambda (k v) (js-count v)))))
        (else       1)))

;; BKB Pathway Provider (currently uses a non-standard query path)
(define url.bkbp-query
  "http://chp.thayer.dartmouth.edu/submitQuery/")

;; Broad Institute KP
(define url.broad
  "https://translator.broadinstitute.org/molepro/trapi/v1.0")

;; Broad Institute KP specific for CHEMBL queries
(define url.broad_chembl
  "https://translator.broadinstitute.org/chembl")

(define chembl-drug-indications/transform
  "/indications/transform")

(define url.unsecret
  "https://unsecret.ncats.io")
(define path.predicates
  "/predicates")
(define path.query
  "/query")

(define (api-query url-string (optional-post-jsexpr (void)))
  (define-values (status headers in)
    (time (if (void? optional-post-jsexpr)
            (http-sendrecv/url
              (string->url url-string)
              #:method "GET")
            (http-sendrecv/url
              (string->url url-string)
              #:method "POST"
              #:data (jsexpr->string optional-post-jsexpr)
              #:headers '("Content-Type: application/json; charset=utf-8")))))
  (define response-string (time (port->string in)))
  ;(pretty-print `(,status ,headers ,response-string))
  (hash 'status status
        'headers headers
        'response (string->jsexpr response-string)))

(define (js-query edges nodes)
  (hash 'message
        (hash 'query_graph
              (hash 'edges edges
                    'nodes nodes))))

(define (js-query/bkbp edges nodes)
  (hash 'query
        (hash 'reasoner_id "unsecret"
              'query_graph
              (hash 'edges edges
                    'nodes nodes))))
(define (bkbp-query edges nodes)
  (api-query url.bkbp-query (js-query/bkbp edges nodes)))

(define (bkbp-g2d e s o)  (hash 'id e  'type "gene_to_disease_association"      'source_id s 'target_id o))
(define (bkbp-d2p e s o)  (hash 'id e  'type "disease_to_phenotype_association" 'source_id s 'target_id o 'value 1000))
(define (bkbp-g id curie) (hash 'id id 'type "Gene"              'curie curie))
(define (bkbp-d id curie) (hash 'id id 'type "disease"           'curie curie))
(define (bkbp-p id curie) (hash 'id id 'type "PhenotypicFeature" 'curie curie))

;{'query':
;  {'query_graph':
;    {'edges':
;      [{'id': 'e0', 'type': 'gene_to_disease_association', 'source_id': 'n0', 'target_id': 'n2'},
;       {'id': 'e1', 'type': 'gene_to_disease_association', 'source_id': 'n1', 'target_id': 'n2'},
;       {'id': 'e2', 'type': 'disease_to_phenotype_association', 'value': 1000, 'source_id': 'n2', 'target_id': 'n3'}],
;     'nodes':
;      [{'id': 'n0', 'type': 'Gene', 'curie':              'ENSEMBL:ENSG00000132155'},
;       {'id': 'n1', 'type': 'Gene', 'curie':              'ENSEMBL:ENSG00000073803'},
;       {'id': 'n2', 'type': 'disease', 'curie':           'MONDO:0007254'},
;       {'id': 'n3', 'type': 'PhenotypicFeature', 'curie': 'EFO:0000714'}]},
;  'reasoner_id': 'unsecret'}}
(define bkbp-edges
  (list (bkbp-g2d "e0" "n0" "n2")
        (bkbp-g2d "e1" "n1" "n2")
        (bkbp-d2p "e2" "n2" "n3")))
(define bkbp-nodes
  (list (bkbp-g "n0" "ENSEMBL:ENSG00000132155")
        (bkbp-g "n1" "ENSEMBL:ENSG00000073803")
        (bkbp-d "n2" "MONDO:0007254")
        (bkbp-p "n3" "EFO:0000714")))
;(define response (bkbp-query bkbp-edges bkbp-nodes))

;
;{"query":
;  {"query_graph":
;    {"edges":
;      [{"target_id":"d00","type":"gene_to_disease_association","source_id":"g00","id":"e00"}],
;     "nodes":
;      [{"type":"Gene","curie":"ENSEMBLE:ENSG00000132155","id":"g00"},
;       {"type":"disease","curie":"MONDO:0007254","id":"d00"}]},
;  "reasoner_id": "unsecret"}}

;; (define RAF1 (hash 'id "g00" 'type "Gene" 'curie "ENSEMBLE:ENSG00000132155"))
;; (define (g2d e s o) (hash 'id e 'type "gene_to_disease_association" 'source_id s 'target_id o))
;; (define breast-cancer (hash 'id "d00" 'type "disease" 'curie "MONDO:0007254"))
;; (define MAP3K13 (hash 'id "g01" 'type "Gene" 'curie "ENSEMBL:ENSG00000073803"))
;; (displayln (jsexpr->string (apply js-query/bkbp (reverse (list (list RAF1 breast-cancer) (list (g2d "e00" "g00" "d00")))))))
;; (bkbp-query (list (g2d "e00" "g00" "d00") (list RAF1 breast-cancer))

;(define (bkb-pathwayo subject subject-type verb object object-type)
  ;(project (subject subject-type verb object object-type)
    ;(let ()
      ;(when (or (var? verb) (var? subject-type) (var? object-type)
                ;(and (var? subject) (var? object)))
        ;(error "verb, types, and subject or object curie must be ground"))
      ;(define edges (list (hash 'id        "e00"
                                ;'source_id "n00"
                                ;'target_id "n01"
                                ;'type      verb)))
      ;(define subject-node
        ;(make-immutable-hash
          ;(append (if (var? subject) '() `((curie . ,subject)))
                  ;`((id . "n00") (type . ,subject-type)))))
      ;(define object-node
        ;(make-immutable-hash
          ;(append (if (var? object) '() `((curie . ,object)))
                  ;`((id . "n01") (type . ,object-type)))))
      ;(define nodes (list subject-node object-node))
      ;(define results
        ;(hash-ref
          ;(hash-ref
            ;(hash-ref (api-query (string-append url.broad path.query)
                                 ;(js-query edges nodes))
                      ;'response)
            ;'knowledge_graph)
          ;'edges))
      ;;(pretty-print results)
      ;;(pretty-print edges)
      ;;(pretty-print nodes)
      ;(let loop ((results results))
        ;(if (null? results) (== #t #f)
          ;(let ((e (car results)))
            ;;; NOTE: this conditional equality constraint is due to the fact
            ;;; that molepro doesn't return the same curies you put in.
            ;(conde ((if (var? subject) (== subject (hash-ref e 'source_id))
                      ;(== #t #t))
                    ;(if (var? object) (== object  (hash-ref e 'target_id))
                      ;(== #t #t)))
                   ;((loop (cdr results))))))))))

(define (molepro-edgeo subject subject-type verb object object-type)
  (project (subject subject-type verb object object-type)
    (let ()
      (when (or (var? verb) (var? subject-type) (var? object-type)
                (and (var? subject) (var? object)))
        (error "verb, types, and subject or object curie must be ground"))
      (define edges (list (hash 'id        "e00"
                                'source_id "n00"
                                'target_id "n01"
                                'type      verb)))
      (define subject-node
        (make-immutable-hash
          (append (if (var? subject) '() `((curie . ,subject)))
                  `((id . "n00") (type . ,subject-type)))))
      (define object-node
        (make-immutable-hash
          (append (if (var? object) '() `((curie . ,object)))
                  `((id . "n01") (type . ,object-type)))))
      (define nodes (list subject-node object-node))
      (define results
        (hash-ref
          (hash-ref
            (hash-ref (api-query (string-append url.broad path.query)
                                 (js-query edges nodes))
                      'response)
            'knowledge_graph)
          'edges))
      ;(pretty-print results)
      ;(pretty-print edges)
      ;(pretty-print nodes)
      (let loop ((results results))
        (if (null? results) (== #t #f)
          (let ((e (car results)))
            ;; NOTE: this conditional equality constraint is due to the fact
            ;; that molepro doesn't return the same curies you put in.
            (conde ((if (var? subject) (== subject (hash-ref e 'source_id))
                      (== #t #t))
                    (if (var? object) (== object  (hash-ref e 'target_id))
                      (== #t #t)))
                   ((loop (cdr results))))))))))

(module+ main
  (pretty-print (api-query (string-append url.broad path.predicates)))

  (run* (g) (molepro-edgeo "CID:2244" "chemical_substance" "affects" g "gene"))
  ;(run 1 (g) (molepro-edgeo "CHEBI:15365" "chemical_substance" "affects" g "gene"))
  ;(run 1 (c) (molepro-edgeo c "chemical_substance" "affects" "HGNC:7645" "gene"))
  (run* (c) (molepro-edgeo "HGNC:7645" "gene" "affected_by" c "chemical_substance"))

  ;(pretty-print
     ;(api-query (string-append url.broad path.query)
              ;(js-query (list (hash 'id        "e00"
                                    ;'source_id "n00"
                                    ;'target_id "n01"
                                    ;'type      "affects"))
                        ;(list (hash 'curie "CID:2244"
                                    ;'id    "n00"
                                    ;'type  "chemical_substance"
                                    ;)
                              ;(hash 'id    "n01"
                                    ;'type  "gene")))))
  )

(module+ old-main
  ;; test predicates available on Broad Institute KG
 #;(pretty-print
   (api-query (string-append url.broad path.predicates)))

  #;(pretty-print
     (api-query (string-append url.broad path.query)
              (js-query (list (hash 'id        "e00"
                                    'source_id "n00"
                                    'target_id "n01"
                                    'type      "affects"))
                        (list (hash 'curie "CID:2244"
                                    'id    "n00"
                                    'type  "chemical_substance")
                              (hash 'id    "n01"
                                    'type  "gene")))))

  #;(js-count
    (time (api-query (string-append url.unsecret path.predicates))))

  #;(pretty-print
    (time
      (api-query (string-append url.unsecret path.query)
                (js-query (list (hash 'id        "e00"
                                       'source_id "n00"
                                       'target_id "n01"
                                       'type      "causes"))
                           (list (hash 'curie "UMLS:C0004096"
                                       'id    "n01"
                                       'type  "disease")
                                 (hash 'id    "n00"
                                       'type  "gene"))))))

  #;(pretty-print
    (time
       (api-query (string-append url.broad path.query)
                  (js-query (list (hash 'id        "e00"
                                        'source_id "n00"
                                        'target_id "n01"
                                        'type      "causes"))
                            (list (hash 'curie "UMLS:C0004096"
                                        'id    "n01"
                                        'type  "disease")
                                  (hash 'id    "n00"
                                        'type  "gene"))))))

  #;(pretty-print
    (time (api-query (string-append url.unsecret path.query)
                     (js-query (list (hash 'id        "e00"
                                           'source_id "n00"
                                           'target_id "n01"
                                           'type      "affects"))
                               (list (hash 'curie "CID:2244"
                                           'id    "n00"
                                           'type  "chemical_substance")
                                     (hash 'id    "n01"
                                           'type  "gene"))))))



#|NOTES FOR SEPTEMBER 2020 RELAY API CALLS|#

#|

1. all "NO-evidence/provenance" queries are to the base broad.url
"https://translator.broadinstitute.org/molepro_reasoner

2. all "LIMITED evidence/provenance" queries are to the chembl url
"https://translator.broadinstitute.org/chembl"

|#

;;MOLEPRO drug --treats--> disease query w/ NO EVIDENCE
(pretty-print
 (time (api-query (string-append url.broad path.query)
                  (js-query (list (hash 'id        "e00"
                                        'source_id "n00"
                                        'target_id "n01"
                                        'type      "treats"))
                            (list (hash 'curie "ChEMBL:CHEMBL25"
                                        'id    "n00"
                                        'type  "chemical_substance")
                                  (hash 'id    "n01"
                                        'type  "disease"))))))

;; new query structure for chembl specific provenance/evidence queries
(define (js-query/transform curie)
  (hash 'collection
        (list
         (hash
          'id ""
          'identifiers
          (hash
           'chembl curie)))
        'controls '()))

#|
;; {} = hash                            ;
;; [] = list                            ;
;; key:value                            ;
                                        ;
QUERY/TRANSFORM QUERY STRUCTURE         ;
------------------------------

query = {
    'collection': [{
            'id':'',
            'identifiers': {'chembl':'ChEMBL:CHEMBL25'}
        }],
    'controls':[]
}
|#

;; addition provenance/evidence for drug indication query
(pretty-print
 (time
  (api-query
   (string-append (string-append url.broad_chembl chembl-drug-indications/transform))
   (js-query/transform
    "ChEMBL:CHEMBL25"))))


;; STRING gene->gene edge
(pretty-print
 (time (api-query (string-append url.broad path.query)
                  (js-query (list (hash 'id        "e00"
                                        'source_id "n00"
                                        'target_id "n01"
                                        'type      "related_to"))
                            (list (hash 'curie "HGNC:4556"
                                        'id    "n00"
                                        'type  "gene")
                                  (hash 'id    "n01"
                                        'type  "gene"))))))

;; CMAP gene-to-compound query
(pretty-print
 (time (api-query (string-append url.broad path.query)
                  (js-query (list (hash 'id        "e00"
                                        'source_id "n00"
                                        'target_id "n01"
                                        'type      "correlated_with"))
                            (list (hash 'curie "HGNC:4556"
                                        'id    "n00"
                                        'type  "gene")
                                  (hash 'id    "n01"
                                        'type  "chemical_substance"))))))

;; CMAP gene-to-gene query
(pretty-print
 (time (api-query (string-append url.broad path.query)
                  (js-query (list (hash 'id        "e00"
                                        'source_id "n00"
                                        'target_id "n01"
                                        'type      "correlated_with"))
                            (list (hash 'curie "HGNC:4556"
                                        'id    "n00"
                                        'type  "gene")
                                  (hash 'id    "n01"
                                        'type  "gene"))))))

;; CMAP gene-to-gene query
(pretty-print
 (time (api-query (string-append url.broad path.query)
                  (js-query (list (hash 'id        "e00"
                                        'source_id "n00"
                                        'target_id "n01"
                                        'type      "correlated_with"))
                            (list (hash 'curie "HGNC:4556"
                                        'id    "n00"
                                        'type  "gene")
                                  (hash 'id    "n01"
                                        'type  "gene"))))))

)
