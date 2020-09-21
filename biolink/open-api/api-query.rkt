#lang racket
(provide (all-defined-out))
(require "../mk.rkt" json net/url)

(define (js-count js)
  (cond ((pair? js) (foldl + 0 (map js-count js)))
        ((hash? js) (foldl + 0 (hash-map js (lambda (k v) (js-count v)))))
        (else       1)))

;; Broad Institute KP
(define url.broad
  "https://translator.broadinstitute.org/molepro_reasoner")

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
  ;(pretty-print `(,status ,headers))
  (hash 'status status
        'headers headers
        'response (string->jsexpr response-string)))

(define (js-query edges nodes)
  (hash 'message
        (hash 'query_graph
              (hash 'edges edges
                    'nodes nodes))))

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
