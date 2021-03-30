#lang racket
(provide (all-defined-out))
(require net/url)
(require net/http-client)
(require json)

(define api-query
  (lambda (api-url)
    (call/input-url
     (string->url api-url)
     get-pure-port
     (lambda (port)
       (string->jsexpr (port->string port))))))

;; ISB KP
(define url-ISB
  "https://biothings.ncats.io/tcga_mut_freq_kp/metadata/fields")

(define ISB-query1
  "https://biothings.ncats.io/drug_response_kp/query?q=subject.SYMBOL:EGFR%20AND%20association.effect_size:%3C0%20AND%20association.pvalue:%3C0.05%20AND%20association.median_ic50_mut:%3C0&size=1000")

(api-query ISB-query1)

#|
TODO: modify Greg's code below to query ISB with structured
contents Subject, Object, pvalue, ic50

;; Broad Institute KP
(define url.broad
  "https://translator.broadinstitute.org/molepro_reasoner")
(define url.unsecret
  "https://unsecret.ncats.io")
(define path.predicates
  "/predicates")
(define path.query
  "/query")

(define (api-query url-string (optional-post-jsexpr (void)))
  (define-values (status headers in)
    (if (void? optional-post-jsexpr)
      (http-sendrecv/url
        (string->url url-string)
        #:method "GET")
      (http-sendrecv/url
        (string->url url-string)
        #:method "POST"
        #:data (jsexpr->string optional-post-jsexpr)
        #:headers '("Content-Type: application/json; charset=utf-8"))))
  (hash 'status status
        'headers headers
        'response (string->jsexpr (port->string in))))

(define (js-query edges nodes)
  (hash 'message
        (hash 'query_graph
              (hash 'edges edges
                    'nodes nodes))))

;; test predicates available on Broad Institute KG
;(pretty-print
  ;(api-query (string-append url.broad path.predicates)))

;(pretty-print
  ;(api-query (string-append url.broad path.query)
             ;(js-query (list (hash 'id        "e00"
                                   ;'source_id "n00"
                                   ;'target_id "n01"
                                   ;'type      "affects"))
                       ;(list (hash 'curie "CID:2244"
                                   ;'id    "n00"
                                   ;'type  "chemical_substance")
                             ;(hash 'id    "n01"
                                   ;'type  "gene")))))

;(pretty-print
  ;(api-query (string-append url.unsecret path.predicates)))

(pretty-print
  (api-query (string-append url.unsecret path.query)
             (js-query (list (hash 'id        "e00"
                                   'source_id "n00"
                                   'target_id "n01"
                                   'type      "affects"))
                       (list (hash 'curie "CHEBI:15365"
                                   'id    "n00"
                                   'type  "chemical_substance")
                             (hash 'id    "n01"
                                   'type  "gene")))))


def buildQuery(genes):
    # empty response
    response = { "query_graph": dict(),
                 "knowledge_graph": dict(),
                 "response": dict()
               }

    # empty query graph
    response["query_graph"] = { "edges": [],
                                "nodes": []
                              }

    # empty knowledge graph
    response["knowledge_graph"] = { "edges": [],
                                    "nodes": []
                                  }

    # empty response graph
    response["results"] = { "node_bindings": [],
                            "edge_bindings": []
                          }

    # nodes
    nodeCount = 0
    # edges
    edgeCount = 0

    # add in evidence genes
    
    for g in genes:
        response['query_graph']['nodes'].append({ 'id':'n{}'.format(nodeCount),
                                                  'type':'Gene',
                                                  'name':'{}'.format(g[0]),
                                                  'curie':'{}'.format(g[1])
                                               })
        nodeCount += 1

    # grouping for genes
    response['query_graph']['nodes'].append({ 'id':'n{}'.format(nodeCount),
                                              'type':'gene_grouping'
                                           })
    nodeCount += 1
    
    # link genes over grouping
    for n in response['query_graph']['nodes'][:-1]:
        response['query_graph']['edges'].append({ 'id':'e{}'.format(edgeCount),
                                                  'type':'part_of',
                                                  'curie':['SEMMEDDB:PART_OF'],
                                                  'source_id':n['id'],
                                                  'target_id':'n{}'.format(nodeCount-1)
                                               })
        edgeCount += 1

    # patient node
    response['query_graph']['nodes'].append({ 'id':'n{}'.format(nodeCount),
                                              'type':'patient',
                                              'curie':['UMLSSC:T101']
                                           })
    nodeCount += 1

    # link gene group to patient
    response['query_graph']['edges'].append({ 'id':'e{}'.format(edgeCount),
                                              'type':'expressed_in',
                                              'curie':['RO:0002206'],
                                              'source_id':'n{}'.format(nodeCount-2),
                                              'target_id':'n{}'.format(nodeCount-1)
                                           })
    edgeCount += 1

    # survival node
    response['query_graph']['nodes'].append({ 'id': 'n{}'.format(nodeCount),
                                              'type': 'PhenotypicFeature',
                                              'curie': 'CHPDART:SURVIVAL',
                                              'operator': '>=',
                                              'value': '1000'
                                           })
    nodeCount += 1

    # link patient to survival
    response['query_graph']['edges'].append({ 'id':'e{}'.format(edgeCount),
                                              'type':'has_phenotype',
                                              'source_id':'n{}'.format(nodeCount-2),
                                              'target_id':'n{}'.format(nodeCount-1)
                                           })
    edgeCount += 1

    # BKB target
    response['probability_targets'] = [('Survival_Time', '>=', 1000)]

    return response






|#
