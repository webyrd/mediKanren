#lang racket/base

(provide get-assoc
         list-assoc
         get-publications
         mvp2-1hop-filter
         mvp2-2hop-filter
         merge-list
         merge-hash
         minus-one-before-zero
         find-max-number
         get-source
         UNSECRET-SOURCE
         num-pubs
         get-score-from-result
         set-score-in-result
         normalize-scores
         edge-has-source?
         publication-attributes
         auxiliary-graph-attribute
         qualifier-attribute*
         merge-trapi-responses
         semantic-exclude*
         domain-exclude*
         range-exclude*
         get-object
         get-and-print-qualifiers
         Unsecret-agent-type-attribute
         Unsecret-knowledge-level-attribute
         agent-type-attribute
         knowledge-level-attribute
         )

(require racket/list
         racket/math
         racket/string
         json
         racket/match
         "../neo-reasoning/semmed-exclude.rkt"
         "../neo-utils/neo-helpers-without-db.rkt"
         )

(define (merge-list xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (merge-list ys (cdr xs)))))

(define (merge-hash h1 h2)
  (define h h2)
  (hash-for-each h1 (lambda (k v) (set! h (hash-set h k v))))
  h)

(define (get-assoc k m)
  (let ((r (assoc k m)))
    (if r
        (cadr r)
        #f)))

(define (list-assoc k m)
  (let ((r (assoc k m)))
    (if r
        (cdr r)
        '())))

(define mvp2-filter
  (lambda (target-eprop direction care-aspect?)
    (let* ((aspect (get-assoc "object_aspect_qualifier" target-eprop))
           (direction^ (get-assoc "object_direction_qualifier" target-eprop)))
      (if care-aspect?
          (and
           aspect
           direction^
           (member aspect '("activity" "abundance" "activity_or_abundance" "expression" "synthesis"))
           (equal? direction direction^))
          (and
           direction^
           (equal? direction direction^))))))

#|
A increases B increases C = A increases C
A decreases B deceases C = A increases C
A increases B decreases C = A decreases C
A decreases B increases C = A decreases C
|#

(define mvp2-2hop-filter
  (lambda (e* direction)
    (filter
     (lambda (e)
       (match e
         [`(,curie_x
            ,pred_xy
            ,curie_y
            ,(? string? pred_yz)
            ,(? string? curie_z)
            ,props_xy
            ,props_yz)
          (if (equal? pred_xy "biolink:affects")
              (if (equal? direction "increased")
                  (or
                   (and (mvp2-filter props_xy "increased" #f)
                        (mvp2-filter props_yz "increased" #t))
                   (and (mvp2-filter props_xy "decreased" #f)
                        (mvp2-filter props_yz "decreased" #t)))
                  (or
                   (and (mvp2-filter props_xy "increased" #f)
                        (mvp2-filter props_yz "decreased" #t))
                   (and (mvp2-filter props_xy "decreased" #f)
                        (mvp2-filter props_yz "increased" #t))))
              (if (equal? direction "increased")
                  (mvp2-filter props_yz "increased" #t)
                  (mvp2-filter props_yz "decreased" #t)))]
         [else #f]))
     e*)))

(define mvp2-1hop-filter
  (lambda (q direction)
    (filter
     (lambda (e)
       (let-values ([(_ eprop) (split-at e 3)])
         (mvp2-filter eprop direction #t)))
     q)))

(define find-max-number
  (lambda (num*)
    (let loop ((n* (cdr num*)) (greatest (car num*)))
      (cond
        ((null? n*) greatest)
        (else
         (if (> (car n*) greatest)
             (loop (cdr n*) (car n*))
             (loop (cdr n*) greatest)))))))

(define (get-source-helper props)
  (get-assoc "primary_knowledge_source" props))

(define (get-source props)
  (hash
   'type "biolink:RetrievalSource"
   'resource_id (get-source-helper props)
   'resource_role "primary_knowledge_source"))

(define (num-pubs props)
  (let ((score (string->number (get-assoc "mediKanren-score" props)))
        (source (get-source-helper props)))
    (cond
      ((equal? source "infores:semmeddb") (* 0.1 score))
      ((equal? source "infores:text-mining-provider-targeted") (* 10 score))
      (else score))))

(define (UNSECRET-SOURCE prop)
  (hash
   'type "biolink:RetrievalSource"
   'resource_id "infores:unsecret-agent"
   'resource_role "aggregator_knowledge_source"
   'upstream_resource_ids (get-assoc "upstream_resource_ids" prop)))

(define (get-score-from-result result)
  (let ((analyses (hash-ref result 'analyses #f)))
    (if analyses
        (hash-ref (car analyses) 'score)
        (error "check the implementation of results.analyses"))))

(define (set-score-in-result result score)
  (let ((analyses (hash-ref result 'analyses #f)))
    (if analyses
        (hash-set result 'analyses
                  (map (lambda (a) (hash-set a 'score score)) analyses))
        (error "check the implementation of results.analyses"))))



(define (normalize-scores results)
  (if (null? results)
      results
      (let ((max-score (get-score-from-result (car results))))
        (if (zero? max-score)
            results
            (map (lambda (x) (set-score-in-result x (/ (get-score-from-result x) (* 1.0 max-score)))) results)))))

(define (edge-has-source? props)
  (get-source-helper props))

(define (publication-attributes props has-pub?)
  (if has-pub?
    (list (get-publications props))
    (list)))

(define get-publications
  (lambda props
    (define (helper props pubs)
      (cond
        [(null? props) pubs]
        [else
         (let ((publication (get-assoc "publications" (car props))))
           (helper (cdr props)
                   (append 
                    (cond
                      [(string-prefix? publication "(")
                       (string-split (string-trim (string-trim publication "(") ")"))] ;rtx-kg2 & robokop
                      [(string-contains? publication "|") (string-split publication "|")] ;text-mining
                      [(string-contains? publication ";") (string-split publication "; ")]
                      [else (string-split publication)])
                    pubs)))]))
    (define pubs (filter
                  (lambda (p) (not (equal? "PMID:" p)))
                  (remove-duplicates (helper props '()))))
    (hash
     'attribute_type_id "biolink:publications"
     'value pubs
     'value_type_id "biolink:Uriorcurie")))

(define auxiliary-graph-attribute
  (lambda (id)
    (hash
     'attribute_type_id "biolink:support_graphs"
     'value (list id))))

(define (qualifier-attribute* qualifier-name props)
  (let ((qualifer (get-assoc qualifier-name props)))
    (if qualifer
        (list (hash 'qualifier_type_id (string-append "biolink:" qualifier-name)
              'qualifier_value qualifer))
        '())))
        

(define Unsecret-agent-type-attribute
  (hash
   'attribute_type_id "biolink:agent_type"
   'value "computational_model"
   'attribute_source "infores:unsecret-agent"))

(define Unsecret-knowledge-level-attribute
  (hash
   'attribute_type_id "biolink:knowledge_level"
   'value "prediction"
   'attribute_source "infores:unsecret-agent"))

(define (agent-type-attribute prop)
  (let* ((edge-from-tmkg? (get-assoc "json_attributes" prop))
         (agent-type (if edge-from-tmkg? "text_mining_agent" (get-assoc "agent_type" prop))))
        (hash
         'attribute_type_id "biolink:agent_type"
         'value agent-type)))

(define (knowledge-level-attribute prop)
  (let* ((edge-from-tmkg? (get-assoc "json_attributes" prop))
         (knowledge-level (if edge-from-tmkg? "not_provided" (get-assoc "knowledge_level" prop))))
        (hash
         'attribute_type_id "biolink:knowledge_level"
         'value knowledge-level)))

;; TODO: test it with calling out Genetics KP
(define (merge-trapi-responses r1 r2 original-query_graph)
  (let* ((message1 (hash-ref r1 'message))
         (message2 (hash-ref r2 'message))
         (auxiliary_graphs1 (hash-ref message1 'auxiliary_graphs))
         (auxiliary_graphs2 (hash-ref message2 'auxiliary_graphs))
         (knowledge_graph1 (hash-ref message1 'knowledge_graph))
         (knowledge_graph2 (hash-ref message2 'knowledge_graph))
         (nodes1 (hash-ref knowledge_graph1 'nodes))
         (nodes2 (hash-ref knowledge_graph2 'nodes))
         (edges1 (hash-ref knowledge_graph1 'edges))
         (edges2 (hash-ref knowledge_graph2 'edges))
         (results1 (hash-ref message1 'results))
         (results2 (hash-ref message2 'results)))
    ;; POSSIBLE TODO
    ;; Might want to check that 'original-query_graph'
    ;; is 'equal?' to the 'query_graph' in 'r1' and the
    ;; 'query_graph' in 'r2' (to ensure we aren't trying
    ;; to merge a response that modifies the 'query_graph'
    ;; in creative mode, for example).
    (hash 'message
          (hash
           ;;
           'query_graph
           original-query_graph
           ;;
           'knowledge_graph
           (hash
            'edges (merge-hash edges1 edges2)
            ;;
            'nodes (merge-hash nodes1 nodes2))
           ;;
           'auxiliary_graphs
           (merge-hash auxiliary_graphs1 auxiliary_graphs2)
           ;; TODO: merge results when they have the same node binding sub/obj depends on mvp
           ;; similar with the metaKG merge
           'results
           (merge-list results1 results2)
           ))))

(define get-object
  (lambda (e)
    (match e
      [`(,score
         ,curie_x
         ,pred_xy
         ,curie_y
         ,(? string? pred_yz)
         ,(? string? curie_z)
         ,props_xy
         ,props_yz)
       curie_z]
      [`(,score
         ,curie_x
         ,pred_xy
         ,curie_y
         .
         ,props_xy)
       curie_y]
      [else (error "invalid form of returned edge" e)])))

(define (get-and-print-qualifiers qualifier)
  (define qualifier-type (hash-ref qualifier 'qualifier_type_id #f))
  (printf "qualifier-type: ~s\n" qualifier-type)
  (define qualifier-value (hash-ref qualifier 'qualifier_value #f))
  (printf "qualifier-value: ~s\n" qualifier-value)
  (list qualifier-type qualifier-value))

  
  
