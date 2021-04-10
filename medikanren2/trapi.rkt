#lang racket/base
(provide trapi-response)
(require
  "common.rkt" "lw-reasoning.rkt"
  racket/file racket/function racket/list racket/hash
  (except-in racket/match ==)
  racket/port
  racket/pretty
  racket/runtime-path
  racket/string
  json
  )

;; QUESTION
;; - Do we return all attributes, or only specified ones?
;; - How is knowledge_graph used in queries?

;; TODO (trapi.rkt and server.rkt)
;; - status - logs - description
;; - Understand Attribute types
;; - QEdge constraints
;; - QNode is_set:
;;       Boolean that if set to true, indicates that this QNode MAY
;;       have multiple KnowledgeGraph Nodes bound to it wi;; thin each
;;       Result. The nodes in a set should be considered as a set of
;;       independent nodes, rather than a set of dependent nodes,
;;       i.e., the answer would still be valid if the nodes in the set
;;       were instead returned individually. Multiple QNodes may have
;;       is_set=True. If a QNode (n1) with is_set=True is connected to
;;       a QNode (n2) with is_set=False, each n1 must be connected to
;;       n2. If a QNode (n1) with is_set=True is connected to a QNode
;;       (n2) with is_set=True, each n1 must be connected to at least
;;       one n2.

(define use-reasoning? (make-parameter #f))

(define trapi-response-node-properties '("category" "name"))
(define trapi-response-node-attributes '(("umls_type_label" . "miscellaneous")
                                         ("umls_type" . "miscellaneous")
                                         ("xrefs" . "miscellaneous")))
(define trapi-response-edge-properties '("predicate" "relation" "subject" "object"))
(define trapi-response-edge-attributes '(("negated" . "miscellaneous")
                                         ("provided_by" . "miscellaneous")
                                         ("publications" . "miscellaneous")))

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

  (fresh (node-bindings edge-bindings)
    (== bindings `((node_bindings . ,node-bindings) 
                   (edge_bindings . ,edge-bindings)))
    ((trapi-nodes nodes k-is-a) node-bindings)
    ((trapi-edges edges k-triple) node-bindings edge-bindings)))

(define (trapi-nodes nodes k-is-a)
  (relation trapi-nodes-o (bindings)
    (let loop ((nodes nodes)
               (bindings bindings))
      (if (null? nodes)
          (== bindings '())
          (let* ((id+n  (car nodes))
                 (id    (car id+n))
                 (n     (cdr id+n))
                 (curies (hash-ref n 'ids #f))
                 (categories (hash-ref n 'categories #f))
                 (constraints (hash-ref n 'constraints '()))
                 (is-set? (hash-ref n 'is_set #f)))
            (if curies
                (if (pair? curies)
                    (fresh (cur k+val bindings-rest)
                      (== bindings `(,k+val . ,bindings-rest))
                      (== k+val `(,id . ,cur))
                      (membero cur curies)
                      ((trapi-constraints constraints) cur)
                      (loop (cdr nodes) bindings-rest))
                    (error "Field: 'ids' must be array of CURIEs (TRAPI 1.1)."))
                  (if (pair? categories)
                      (fresh (cat var k+var bindings-rest)
                        (== k+var `(,id . ,var));)
                        ((trapi-constraints constraints) var)
                        (== bindings `(,k+var . ,bindings-rest))
                        (membero cat categories)
                        (conde ((is-a var cat))
                               ((k-is-a var cat)))
                        (loop (cdr nodes) bindings-rest))
                      (error "Field: 'QNode/categories' must be array of CURIESs (TRAPI 1.1)."))))))))
                    
(define (trapi-edges edges k-triple)
  (relation trapi-edges-o (node-bindings edge-bindings)
    (let loop ((edges edges) (bindings edge-bindings))
      (if (null? edges)
          (== bindings '()) 
          (let* ((id+e      (car edges))
                 (id        (car id+e))
                 (e         (cdr id+e))
                 (predicates (hash-ref e 'predicates #f))
                 (subject   (string->symbol (hash-ref e 'subject #f)))
                 (relation  (hash-ref e 'relation #f))
                 (object    (string->symbol (hash-ref e 'object #f)))
                 (constraints (hash-ref e 'constraints '())))
            (if (and predicates (not (pair? predicates)))
                (error "Field: 'QEdge/predicates' must be an array of CURIEs (TRAPI 1.1).")
                (fresh (db+eid s p o bindings-rest)
                  (membero `(,subject . ,s) node-bindings)
                  (membero `(,object . ,o) node-bindings)
                  (conde ((== predicates #f)) 
                         ((membero p predicates)))
                  (conde ((edge db+eid s o)
                          (edge-predicate/lwr db+eid p)
                          (conde ((== relation #f))
                                 ((stringo relation) 
                                  (eprop db+eid "relation" relation))))
                         ((fresh (eid)
                            (k-triple eid s p o)
                            (== db+eid `(kg . ,eid)))))
                  (== bindings `((,id . ,db+eid) . ,bindings-rest))
                  (loop (cdr edges) bindings-rest))))))))

(define (trapi-constraints constraints)
  (relation trapi-node-constraints-o (node)
    (let loop ((constraints constraints))
      (if (null? constraints)
          (== #t #t)
          (let* ((constraint (car constraints))
                 (id        (hash-ref constraint 'id #f))
                 (not?      (hash-ref constraint 'not #f))
                 (operator  (hash-ref constraint 'operator #f))
                 (enum      (hash-ref constraint 'enum #f))
                 (value     (hash-ref constraint 'value #f))
                 (unit-id   (hash-ref constraint 'unit_id #f))) ;?
            (fresh (val)
              (triple node id val)
              (case operator
                ((">")  (if not?
                          (<=o val value)
                          (<o value val)))
                (("<")  (if not?
                          (<=o val value)
                          (<o val value)))
                (("matches") (:== #t (val) (regexp-match value val)))
                (else (if not?
                          (=/= val value)
                          (== val value))))
              (loop (cdr constraints))))))))

(define (trapi-response msg)
  (define results (run* bindings (trapi-query msg bindings)))
  (hash 'results (trapi-response-results results)
        'knowledge_graph
        (hash 'nodes (trapi-response-knodes results)
              'edges (trapi-response-kedges results))))

(define (edge-id/reported db+eid)
  (let ((db     (car db+eid))
        (eid    (cdr db+eid)))
    (if (eq? db 'kg) (strlift eid) 
        (string-append (strlift db) "." (strlift eid)))))

(define (trapi-response-results results)
  (map (lambda (bindings)
         (hash 'node_bindings
               (make-hash
                (map (lambda (binding) 
                       `(,(car binding) ,(hash 'id (cdr binding))))
                     (alist-ref bindings 'node_bindings '())))
               'edge_bindings
               (make-hash
                (map (lambda (ebinding)
                       (let ((db+id (cdr ebinding)))
                         `(,(car ebinding) ,(hash 'id (edge-id/reported db+id)))))
                     (alist-ref bindings 'edge_bindings '())))) )
       results))

(define (unique-bindings-values results key)
  (remove-duplicates
   (apply append
          (map
           (lambda (nb) (map cdr (alist-ref nb key #f)))
           results))))

(define (props-kv k+v) (cons (string->symbol (car k+v)) (cdr k+v)))
(define (attributes-kv keys)
  (lambda (k+v)
    (let ((k (car k+v)) (v (cdr k+v)))
      (make-hash
       `((attribute_type_id . ,(alist-ref keys k "miscellaneous"))
         (value_type_type . ,(alist-ref keys k "miscellaneous"))
         (original_attribute_name . ,(strlift k))
         (value . ,(strlift v)))))))

(define (trapi-response-knodes/edges results key new-key query attributes-query attributes-keys)
  (make-hash
   (map (lambda (node)
          `(,(new-key node)
            . ,(make-hash
                (append (let ((attributes (attributes-query node)))
                          (if (pair? attributes)
                              `((attributes . ,(map (attributes-kv attributes-keys) attributes))) '()))
                        (map props-kv (query node))))))
        (unique-bindings-values results key))))

(define (trapi-response-knodes results)
  (trapi-response-knodes/edges
   results 'node_bindings string->symbol
   (lambda (node) 
     (run* prop
       (fresh (k v)
         (membero k trapi-response-node-properties)
         (== `(,k . ,v) prop)
         (cprop node k v))))
   (lambda (node)
     (run* attribute
      (fresh (k v)
        (membero k (map car trapi-response-node-attributes))
        (== `(,k . ,v) attribute)
        (cprop node k v))))
   trapi-response-node-attributes))

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
   results 'edge_bindings (compose symlift edge-id/reported)
   (lambda (node) 
     (run* prop
       (fresh (k v)
         (membero k trapi-response-edge-properties)
         (== `(,k . ,v) prop)
         (eprop node k v))))
   (lambda (node)
     (run* attribute
      (fresh (k v)
        (membero k (map car trapi-response-edge-attributes))
        (== `(,k . ,v) attribute)
        (eprop node k v))))
   trapi-response-edge-attributes))

