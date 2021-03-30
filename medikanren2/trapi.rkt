#lang racket/base
(provide trapi-response)
(require
  "common.rkt"
  racket/file racket/function racket/list racket/hash
  (except-in racket/match ==)
  racket/port
  racket/pretty
  racket/runtime-path
  racket/string
  json
  )

;; (define q (with-input-from-file "trapi-parsing-test.json" read-json))

(define q (string->jsexpr #<<EOS
{
    "message": {
        "query_graph": {
            "edges": {
                "e00": {
                    "subject": "n00",
                    "object": "n01",
                    "predicate": "biolink:gene_associated_with_condition"
                }
            },
            "nodes": {
                "n00": {
                    "id" : "UniProtKB:P51587",
                    "category": "biolink:biological_entity"
                },
                "n01": {
                    "category": "biolink:Disease"
                }
            }
        }
    }
}
EOS
))

;; example using knowledge_graph
(define q2 (string->jsexpr #<<EOS
{
    "message": {
        "query_graph": {
            "edges": {
                "e00": {
                    "subject": "n00",
                    "object": "n01",
                    "predicate" : "biolink:treats"
                }
            },
            "nodes": {
                "n00": {
                    "id" : "CHEBI:6801XXX"
                },
                "n01": {
                    "category": "biolink:Disease"
                }
            }
        },
       "knowledge_graph" : {
           "nodes": {
               "MONDO:0005148": {"name": "type-2 diabetes", "category":"biolink:Disease"},
               "CHEBI:6801XXX": {"name": "metformin", "category": "drug"}
            },
           "edges": {
              "df87ff82": {"subject": "CHEBI:6801XXX", "predicate": "biolink:treats", "object": "MONDO:0005148"}
            }
         }
    }
}
EOS
))
(define (alist-ref alist key default)
  (define kv (assoc key alist))
  (if kv (cdr kv) default))

(define hash-empty (hash))
(define (str   v) (if (string? v) v (error "invalid string:" v)))
(define (olift v) (if (hash?   v) v (error "invalid object:" v)))
(define (slift v) (cond ((pair?   v) v)
                        ((string? v) (list v))
                        ((null?   v) '())
                        (else        (error "invalid string or list of strings:" v))))
(define (alist-of-hashes->lists v)
  (map (lambda (pair)
         (cons (car pair) (hash->list (olift (cdr pair)))))
       v))

(define (trapi-query msg bindings)
  (define qgraph (hash-ref msg 'query_graph))
  (define nodes  (hash->list (olift (hash-ref qgraph 'nodes hash-empty))))
  (define edges  (hash->list (olift (hash-ref qgraph 'edges hash-empty))))

  ;; TRAPI knowledge_graph (in progress)
  (define kgraph (hash-ref msg 'knowledge_graph #f))
  (define knodes (and kgraph
                      (alist-of-hashes->lists
                       (hash->list (olift (hash-ref kgraph 'nodes hash-empty))))))
  (define kedges (and kgraph (alist-of-hashes->lists
                              (hash->list (olift (hash-ref kgraph 'edges hash-empty))))))
  (define-relation (k-is-a concept category)
    (fresh (props concept-sym category-sym)
      (membero `(,concept-sym . ,props) knodes)
      (membero `(category . ,category) props)
      (:== concept-sym (concept) (string->symbol concept))
      (:== category-sym (category) (string->symbol category))))
  (define-relation (k-triple eid s p o)
    (fresh (props)
      (membero `(,eid . ,props) kedges)
      (membero `(subject . ,s) props)
      (membero `(predicate . ,p) props)
      (membero `(object . ,o) props)))

(define-relation (trapi-nodes nodes bindings)
  (let loop ((nodes nodes)
             (bindings bindings))
    (if (null? nodes)
        (== bindings '())
        (let* ((id+n  (car nodes))
               (id    (car id+n))
               (n     (cdr id+n))
               (curie (hash-ref n 'id #f))
               (category (hash-ref n 'category #f)))
          (cond (curie
                 (fresh (k+val bindings-rest)
                   (== k+val `(,id . ,curie))
                   (== bindings `(,k+val . ,bindings-rest))
                   (loop (cdr nodes) bindings-rest)))
                (category
                 (fresh (var k+var bindings-rest)
                   (== k+var `(,id . ,var))
                   (== bindings `(,k+var . ,bindings-rest))
                   (conde ((is-a var category))
                          ((k-is-a var category)))
                   (loop (cdr nodes) bindings-rest))))))))

(define-relation (trapi-edges edges node-bindings edge-bindings)
  (let loop ((edges edges) (bindings edge-bindings))
    (if (null? edges)
        (== bindings '())
        (let* ((id+e      (car edges))
               (id        (car id+e))
               (e         (cdr id+e))
               (predicate (hash-ref e 'predicate #f))
               (subject   (string->symbol (hash-ref e 'subject #f)))
               (relation  (hash-ref e 'relation #f))
               (object    (string->symbol (hash-ref e 'object #f))))
          (fresh (db+eid s p o bindings-rest)
            (membero `(,subject . ,s) node-bindings)
            (membero `(,object . ,o) node-bindings)
            (conde ((== predicate #f)) 
                   ((stringo predicate) (== p predicate)))
            (conde ((edge db+eid s o))
                   ((fresh (eid)
                      (k-triple eid s p o)
                      (== db+eid `(kg . ,eid)))
                    ))
            (== bindings `((,id . ,db+eid) . ,bindings-rest))
            (loop (cdr edges) bindings-rest))))))

  (fresh (node-bindings edge-bindings)
    (== bindings `((node_bindings . ,node-bindings) 
                   (edge_bindings . ,edge-bindings)))
    (trapi-nodes nodes node-bindings)
    (trapi-edges edges node-bindings edge-bindings)))



(define (trapi-response msg)
  (define results (run 1 bindings (trapi-query msg bindings)))
  (hash 'message
        (hash 'results (trapi-response-results results)
              'knowledge_graph
              (hash 'nodes (trapi-response-knodes results)
                    'edges (trapi-response-kedges results)))))

(define (trapi-response-results results)
  (map (lambda (bindings)
         (hash 'node_bindings 
               (map (lambda (binding) (hash (car binding)
                                            (list (hash 'id (cdr binding)))))
                    (alist-ref bindings 'node_bindings '()))               
               
               'edge_bindings
               (map (lambda (ebinding)
                      (let* ((db+eid (cdr ebinding))
                             (db     (car db+eid))
                             (eid    (strlift (cdr db+eid))))
                        (hash (car ebinding) (list (hash 'id eid)))))
                    (alist-ref bindings 'edge_bindings '()))))
       results))

(define (unique-bindings-values results key)
  (remove-duplicates
   (apply append
          (map
           (lambda (nb) (map cdr (alist-ref nb key #f)))
           results))))

(define (props-kv k+v) (cons (string->symbol (car k+v)) (cdr k+v)))
(define trapi-response-node-properties '("category" "name"))

(define (trapi-response-knodes/edges results key new-key query)
  (make-hash
   (map (lambda (node)
          `(,(new-key node)
            . ,(make-hash
                (map props-kv (query node)))))
        (unique-bindings-values results key))))

(define (trapi-response-knodes results)
  (trapi-response-knodes/edges
   results 'node_bindings string->symbol
   (lambda (node) 
     (run* prop
       (fresh (k v)
         (membero k trapi-response-node-properties)
         (== `(,k . ,v) prop)
         (cprop node k v))))))

(define (symlift v) 
  (cond ((number? v) (string->symbol (number->string v)))
        ((string? v) (string->symbol v))
        ((symbol? v) v)
        (else (error "Must be a numebr, string or symbol: ~s." v))))

(define (strlift v)
  (cond ((number? v) (number->string v))
        ((string? v) v)
        ((symbol? v) (symbol->string v))
        (error "Must be a number, string or symbol: ~s" v)))

(define (trapi-response-kedges results)
  (trapi-response-knodes/edges 
   results 'edge_bindings (compose symlift cdr)
   (lambda (node) 
     (run* prop
       (fresh (k v)
         (== `(,k . ,v) prop)
         (eprop node k v))))))

(define m1    (hash-ref q 'message))
(define m2    (hash-ref q2 'message))

(define r (trapi-response m1))
(display (jsexpr->string r))

(define r2 (trapi-response m2))
(display (jsexpr->string r2))

