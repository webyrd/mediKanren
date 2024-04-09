#lang racket

(require
  net/url
  json
  rackunit)

(define MEDIKANREN-LOCAL-URL (string->url "http://127.0.0.1:8384/query"))

(define HEADER
  (list "accept: application/json" 
        "Content-Type: application/json"))

(define build-mvp-query
  (lambda (which-mvp known-id direction)
    (cond
      [(eq? which-mvp 'mvp1)
       (hash 'message
             (hash 
              'disable_external_requests "true"
              'query_graph (hash
                            'nodes (hash 
                                    'n0 (hash
                                         'ids (list known-id)
                                         'categories (list "biolink:Disease"))
                                    'n1 (hash
                                         'categories (list "biolink:ChemicalEntity")))
                            'edges (hash
                                    'e0 (hash
                                         'subject "n1"
                                         'object "n0"
                                         'predicates (list "biolink:treats")
                                         'knowledge_type "inferred")))))]
      [(eq? which-mvp 'mvp2-gene)
       (hash 'message
             (hash
              'disable_external_requests "true"
              'query_graph
              (hash 'edges
                    (hash 't_edge
                          (hash 'knowledge_type "inferred"
                                'object "gene"
                                'predicates (list "biolink:affects")
                                'qualifier_constraints
                                (list (hash
                                       'qualifier_set
                                       (list
                                        (hash 'qualifier_type_id "biolink:object_aspect_qualifier"
                                              'qualifier_value "activity_or_abundance")
                                        (hash 'qualifier_type_id "biolink:object_direction_qualifier"
                                              'qualifier_value direction))))
                                'subject "chemical"))
                    'nodes (hash
                            'chemical (hash
                                       'categories (list "biolink:ChemicalEntity"))
                            'gene (hash 'categories (list "biolink:Gene")
                                        'ids (list known-id))))))]
      [(eq? which-mvp 'mvp2-chem)
       (hash 'message
             (hash
              'disable_external_requests "true"
              'query_graph
              (hash 'edges
                    (hash 't_edge
                          (hash 'knowledge_type "inferred"
                                'object "gene"
                                'predicates (list "biolink:affects")
                                'qualifier_constraints
                                (list (hash
                                       'qualifier_set
                                       (list
                                        (hash 'qualifier_type_id "biolink:object_aspect_qualifier"
                                              'qualifier_value "activity_or_abundance")
                                        (hash 'qualifier_type_id "biolink:object_direction_qualifier"
                                              'qualifier_value direction))))
                                'subject "chemical"))
                    'nodes (hash
                            'chemical (hash
                                       'categories (list "biolink:ChemicalEntity")
                                       'ids (list known-id))
                            'gene (hash 'categories (list "biolink:Gene")
                                        )))))]
      [else (error "unknown mvp")])))


;; TODO: how to get the body and http_code for one call
;; https://gist.github.com/branneman/8a018e39e993328be1cf25cf9035e7d6
(define get-response
  (lambda (which-mvp known-id direction)
    (call/input-url
     MEDIKANREN-LOCAL-URL
     (lambda (url) (post-pure-port
                    url
                    (jsexpr->bytes (build-mvp-query which-mvp known-id direction))
                    HEADER))
     read-json)))

(define (check-X-in-list X)
    (lambda (lt) (member X lt)))

(define (missing-string x)
  (if (string? x)
      (string-append x " is missing")
      (error "x is missing and x should be a string")))

(define (test-reply response)

  (check-pred (check-X-in-list 'message) (hash-keys response) (missing-string "\"message\""))
  (define message-from-response (hash-ref response 'message (hash)))
  (define message-key* (hash-keys message-from-response))
  (check-pred (check-X-in-list 'results) message-key* (missing-string "\"results\""))
  (check-pred (check-X-in-list 'query_graph) message-key* (missing-string "\"query_graph\""))
  (check-pred (check-X-in-list 'knowledge_graph) message-key* (missing-string "\"knowledge_graph\""))
  (check-pred (check-X-in-list 'auxiliary_graphs) message-key* (missing-string "\"auxiliary_graphs\""))


  (define query-graph-from-response (hash-ref message-from-response 'query_graph (hash)))
  (define query-graph-key* (hash-keys query-graph-from-response))
  (check-pred (check-X-in-list 'nodes) query-graph-key* (missing-string "\"query_graph.nodes\""))
  (check-pred (check-X-in-list 'edges) query-graph-key* (missing-string "\"query_graph.edges\""))

  (define Qedge-from-response (hash-ref query-graph-from-response 'edges (hash)))
  (for-each
   (lambda (Qedge-key)
     (define Qedge-dic (hash-ref Qedge-from-response Qedge-key))
     (check-pred (check-X-in-list 'subject) (hash-keys Qedge-dic) (missing-string "\"query_graph.edges.subject\""))
     (check-pred (check-X-in-list 'object) (hash-keys Qedge-dic) (missing-string "\"query_graph.edges.object\""))
     )
   (hash-keys Qedge-from-response))

  (define auxiliary-graphs-from-response (hash-ref message-from-response 'auxiliary_graphs (hash)))
  (for-each
   (lambda (auxiliary_graph-key)
     (define auxiliary_graph (hash-ref auxiliary-graphs-from-response auxiliary_graph-key))
     (check-pred (check-X-in-list 'edges) (hash-keys auxiliary_graph) (missing-string "\"auxiliary_graphs.edges\"")))
   (hash-keys auxiliary-graphs-from-response))

  (define knowledge-graph-from-response (hash-ref message-from-response 'knowledge_graph (hash)))
  (define knowledge-graph-key* (hash-keys knowledge-graph-from-response))
  (check-pred (check-X-in-list 'nodes) knowledge-graph-key* (missing-string "\"knowledge_graph.nodes\""))
  (check-pred (check-X-in-list 'edges) knowledge-graph-key* (missing-string "\"knowledge_graph.edges\""))

  (define Kedge-from-response (hash-ref knowledge-graph-from-response 'edges (hash)))
  (for-each
   (lambda (Kedge-key)
     (define Kedge-dic (hash-ref Kedge-from-response Kedge-key))
     (check-pred (check-X-in-list 'subject) (hash-keys Kedge-dic) (missing-string "\"knowledge_graph.edges.subject\""))
     (check-pred (check-X-in-list 'object) (hash-keys Kedge-dic) (missing-string "\"knowledge_graph.edges.object\""))
     (check-pred (check-X-in-list 'predicate) (hash-keys Kedge-dic) (missing-string "\"knowledge_graph.edges.predicate\""))
     (check-pred (check-X-in-list 'sources) (hash-keys Kedge-dic) (missing-string "\"knowledge_graph.edges.sources\"")))
   (hash-keys Kedge-from-response))

  (define result*-from-response (hash-ref message-from-response 'results '()))
  (for-each 
   (lambda (result-from-response)
     (define results-key* (hash-keys result-from-response))
     (check-pred (check-X-in-list 'node_bindings) results-key* (missing-string "\"results.node_bindings\""))
     (check-pred (check-X-in-list 'analyses) results-key* (missing-string "\"results.analyses\""))
     (for-each
      (lambda (analyses)
        (define analyses-key* (hash-keys analyses))
        (check-pred (check-X-in-list 'resource_id) analyses-key*
                    (missing-string "\"results.analyses.resource_id\""))
        (check-pred (check-X-in-list 'edge_bindings) analyses-key*
                    (missing-string "\"results.analyses.edge_bindings\""))
        (define edge_bindings (hash-ref analyses 'edge_bindings (hash)))
        (for-each
         (lambda (edge-binding-key)
           (define edge_binding* (hash-ref edge_bindings edge-binding-key))
           (for-each
            (lambda (edge_binding)
              (check-pred (check-X-in-list 'id) (hash-keys edge_binding) (missing-string "\"results.analyses.edge_bindings.id\"")))
            edge_binding*))
         (hash-keys edge_bindings))
        (define node_bindings (hash-ref result-from-response 'node_bindings))
        (for-each
         (lambda (node-binding-key)
           (define node-binding* (hash-ref node_bindings node-binding-key))
           (for-each
            (lambda (node-binding)
              (check-pred (check-X-in-list 'id) (hash-keys node-binding)
                          (missing-string "\"results.node_bindings.id\"")))
            node-binding*))
         (hash-keys node_bindings)))
      (hash-ref result-from-response 'analyses '()))
     )
   result*-from-response)
  )

(define (test-and-timer which-mvp known-id)
  (printf "About to call the ~a query with input id ~a\n" which-mvp known-id)
  (if (or (eq? which-mvp 'mvp2-chem) (eq? which-mvp 'mvp2-gene))
      (begin
        (printf "increased: ")
        (test-reply (time (get-response which-mvp known-id "increased")))
        (printf "decreased: ")
        (test-reply (time (get-response which-mvp known-id "decreased"))))
      (test-reply (time (get-response which-mvp known-id #f)))))


(test-and-timer 'mvp2-chem "PUBCHEM.COMPOUND:3007") ;Amphetamine ;Amphetamine
(test-and-timer 'mvp2-chem "PUBCHEM.COMPOUND:5826") ;Dextroamphetamine
(test-and-timer 'mvp2-chem "PUBCHEM.COMPOUND:44246724") ;Methylphenidate
(test-and-timer 'mvp2-gene "NCBIGene:1594") ;P450
(test-and-timer 'mvp2-gene "NCBIGene:3075") ;FHL1
(test-and-timer 'mvp2-gene "NCBIGene:7157") ;TP53
(test-and-timer 'mvp1 "MONDO:0005148") ;"type 2 diabetes mellitus"
(test-and-timer 'mvp1 "MONDO:0005302") ;"attention deficit hyperactivity disorder, inattentive type"
(test-and-timer 'mvp1 "MONDO:0018956") ;"idiopathic bronchiectasis"
(test-and-timer 'mvp1 "MONDO:0007254") ;breast cancer
(test-and-timer 'mvp1 "MONDO:0005147") ;type 1 diabetes mellitus
(test-and-timer 'mvp1 "MONDO:0020066") ;Ehlers-Danlos syndrome
(test-and-timer 'mvp1 "MONDO:0007827") ;inclusion body myositis
(test-and-timer 'mvp1 "MONDO:0001302") ;hypertensive heart disease