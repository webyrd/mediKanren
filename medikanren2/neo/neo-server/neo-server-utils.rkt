#lang racket/base

(provide get-assoc
         list-assoc
         mvp2-1hop-filter
         mvp2-2hop-filter
         auto-grow
         merge-list
         merge-hash
         minus-one-before-zero
         find-max-number
         get-source
         num-pubs
         get-score-from-result
         set-score-in-result
         normalize-scores
         edge-has-source?
         data-attributes
         auxiliary-graph-attribute
         merge-trapi-responses
         semantic-exclude*
         domain-exclude*
         range-exclude*
         get-object
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
  (lambda (target-eprop direction)
    (let* ((aspect (or (get-assoc "object_aspect_qualifier" target-eprop)
                       (get-assoc "qualified_object_aspect" target-eprop)
                       ))
           (direction^ (or (get-assoc "object_direction_qualifier" target-eprop)
                           (get-assoc "qualified_object_direction" target-eprop)
                           )))
      (and
       aspect
       direction^
       (or
        (equal? "activity" aspect)
        (equal? "abundance" aspect)
        (equal? "activity_or_abundance" aspect)
        (equal? "expression" aspect)
        (equal? "synthesis" aspect))
       (equal? direction direction^)))))

(define mvp2-2hop-filter
  (lambda (q direction)
    (filter
     (lambda (e)
       (let-values ([(_ eprop) (split-at e 5)])
         (mvp2-filter (cadr eprop) direction)))
     q)))

(define mvp2-1hop-filter
  (lambda (q direction)
    (filter
     (lambda (e)
       (let-values ([(_ eprop) (split-at e 3)])
         (mvp2-filter eprop direction)))
     q)))


(define (auto-grow hop-proc score* result_amount)
  (let ((half-result (exact-round (/ result_amount 2.0))))
    (let loop ((r '()) (sl score*))
      (cond
        [(> (length r) half-result)
         (printf "return ~a answers\n" (length r))
         r]
        [(andmap not sl)
         (printf "return ~a answers\n" (length r))
         r]
        [else
         (loop (append r (hop-proc sl))
               (list (minus-one-before-zero (list-ref sl 0))
                     (minus-one-before-zero (list-ref sl 1))
                     (minus-one-before-zero (list-ref sl 2))))]))))

(define find-max-number
  (lambda (num*)
    (let loop ((n* (cdr num*)) (greatest (car num*)))
      (cond
        ((null? n*) greatest)
        (else
         (if (> (car n*) greatest)
             (loop (cdr n*) (car n*))
             (loop (cdr n*) greatest)))))))

(define (get-source props)
  (let ((source (or (get-assoc "biolink:primary_knowledge_source" props)
                    (get-assoc "primary_knowledge_source" props) ;rkx-kg2
                    (and (get-assoc "json_attributes" props)
                         "infores:text-mining-provider-targeted")))) ;text-mining
    (hash
      'resource_id source
      'resource_role "primary_knowledge_source")))

(define (num-pubs props)
  (let ((pubs (or (get-assoc "publications" props)
                  (get-assoc "supporting_publications" props)
                  (get-assoc "publications:string[]" props))))
    (if (and pubs (not (equal? "()" pubs)))
        (max (length (string-split pubs "|")) (length (string-split pubs "; ")) (length (string-split pubs)))
        0)))

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
        (map (lambda (x) (set-score-in-result x (/ (get-score-from-result x) (* 1.0 max-score)))) results))))

(define edge-has-source?
  (lambda (props)
    (or (get-assoc "biolink:primary_knowledge_source" props)
        (get-assoc "primary_knowledge_source" props)
        (and (get-assoc "json_attributes" props)
             (let ((attr-hl (string->jsexpr (get-assoc "json_attributes" props))))
               (let loop ((hl attr-hl))
                 (cond
                   ((null? hl) #f)
                   ((equal?
                     (hash-ref (car hl) 'attribute_type_id #f)
                     "biolink:primary_knowledge_source")
                    #t)
                   (else (loop (cdr hl)))))))
        (get-assoc "knowledge_source" props))))

(define (data-attributes props)
    (list (get-publications props)))

(define get-publications
  (lambda props
    (define (helper props pubs)
      (cond
        [(null? props) pubs]
        [else
         (let ((publication (or (get-assoc "publications" (car props))
                                (get-assoc "supporting_publications" (car props))
                                (get-assoc "publications:string[]" (car props)))))
           (helper (cdr props)
                   (append 
                    (cond
                      [(string-prefix? publication "(")
                       (string-split (string-trim (string-trim publication "(") ")"))]
                      [(string-contains? publication "|") (string-split publication "|")]
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


  
  
