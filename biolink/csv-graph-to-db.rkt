#lang racket/base
(require
  "csv.rkt"
  "repr.rkt"
  json
  racket/match
  )

(define (json-simplify json)
  (if (and (string? json) (< 0 (string-length json))
           (eqv? #\" (string-ref json 0)))
    (string->jsexpr json)
    json))

(define argv (current-command-line-arguments))
(define argv-expected '#(DATA_DIR GRAPH_DIR))

(when (not (= (vector-length argv-expected) (vector-length argv)))
  (error "command line argument mismatch:" argv-expected argv))

(define data-dir (vector-ref argv 0))
(define graph-dir (vector-ref argv 1))
(define (graph-path fname)
  (expand-user-path (build-path data-dir graph-dir fname)))

;; Input
;; We don't need anything from *.node.csv to build the DB.
(define fnin-nodeprop (string-append graph-dir ".nodeprop.csv"))
(define fnin-edge     (string-append graph-dir ".edge.csv"))
(define fnin-edgeprop (string-append graph-dir ".edgeprop.csv"))

;; Output
(define fnout-concepts             "concepts.scm")
(define fnout-categories           "categories.scm")
(define fnout-concepts-by-category "concepts-by-category.scm")
(define fnout-edges                "edges.scm")
(define fnout-predicates           "predicates.scm")
(define fnout-edges-by-subject     "edges-by-subject.bytes")
(define fnout-edges-by-object      "edges-by-object.bytes")
(define (fname-offset fname) (string-append fname ".offset"))

(define nodeprop-header-expected ":ID,propname,value")
(define edgeprop-header-expected ":ID,propname,value")
(define edge-header-expected     ":ID,:START,:END")
(define (validate-header header-expected in)
  (define header-found (read-line in 'any))
  (when (not (equal? header-found header-expected))
    (error "unexpected CSV header:" header-found header-expected)))

(define cui=>id&cat (hash))

(define (process-nodes in-nodeprop out-concepts out-offset-concepts
                       out-categories out-concepts-by-category
                       out-offset-concepts-by-category)
  (define (flush)
    (flush-output out-categories)
    (flush-output out-concepts)
    (flush-output out-offset-concepts))
  (define category-count 0)
  (define category=>id (hash))
  (define cat-id=>concept-id* (hash))
  (define add-concept
    (let ((id 0))
      (define (add-category category)
        (cond ((not (hash-has-key? category=>id category))
               (set! category=>id
                 (hash-set category=>id category category-count))
               (detail-write out-categories #f category)
               (set! category-count (+ category-count 1))
               (- category-count 1))
              (else (hash-ref category=>id category))))
      (lambda (cui props)
        (define (required-prop props key)
          (define kv (assoc key props))
          (and kv (cdr kv)))
        (define category (required-prop props "category"))
        (define name (required-prop props "name"))
        (define category-id (add-category category))
        (define (other-key? kv)
          (not (ormap (lambda (k) (equal? k (car kv))) '("category" "name"))))
        (set! cui=>id&cat (hash-set cui=>id&cat cui (cons id category-id)))
        (set! cat-id=>concept-id*
          (hash-set cat-id=>concept-id*
                    category-id
                    (cons id (hash-ref cat-id=>concept-id* category-id '()))))
        (detail-write out-concepts out-offset-concepts
                      (vector cui category-id name (filter other-key? props)))
        (set! id (+ id 1)))))

  (define nodeprops (csv-records in-nodeprop))
  (define first-nodeprop (nodeprops 'next))
  (when (not first-nodeprop) (error "nodeprop file is empty"))
  (match-define (list cui key raw-val) first-nodeprop)

  (let loop ((count 1) (current-cui cui))
    (define props
      (let loop-props ((row (nodeprops 'current)))
        (match row
          ((list (? (lambda (cui) (equal? current-cui cui))) key raw-val)
           (cons (cons key (json-simplify raw-val))
                 (loop-props (nodeprops 'next))))
          (_ '()))))
    (add-concept current-cui props)
    (when (= 0 (remainder count 10000))
      (printf "Processed ~s nodes/concepts\n" count)
      (flush))

    (match (nodeprops 'current)
      ((list cui _ _) (loop (+ 1 count) cui))
      (#f (printf "Found ~s concepts in ~s categories\n"
                  (- count 1) category-count)
       (flush)
       (for ((cat-id (in-range 0 category-count)))
            (define cids (sort (hash-ref cat-id=>concept-id* cat-id '()) <=))
            (define vcids (list->vector cids))
            (printf "Category ~s: ~s concepts\n" cat-id (vector-length vcids))
            (detail-write
              out-concepts-by-category out-offset-concepts-by-category vcids)))
      (_ (error "malformed row:" (nodeprops 'current))))))

(define (process-edges in-edge in-edgeprop
                       out-predicates
                       out-edges out-offset-edges
                       out-edges-by-subject out-offset-edges-by-subject
                       out-edges-by-object  out-offset-edges-by-object)
  (define (flush)
    (flush-output out-predicates)
    (flush-output out-edges)
    (flush-output out-offset-edges))
  (define predicate-count 0)
  (define predicate=>id (hash))
  (define subject=>edges (hash))
  (define object=>edges (hash))
  (define add-edge
    (let ((id 0))
      (define (add-predicate predicate)
        (cond ((not (hash-has-key? predicate=>id predicate))
               (set! predicate=>id
                 (hash-set predicate=>id predicate predicate-count))
               (detail-write out-predicates #f predicate)
               (set! predicate-count (+ predicate-count 1))
               (- predicate-count 1))
              (else (hash-ref predicate=>id predicate))))
      (lambda (subject-cui object-cui props)
        (match-define (cons subject subject-category)
                      (hash-ref cui=>id&cat subject-cui))
        (match-define (cons object object-category)
                      (hash-ref cui=>id&cat object-cui))
        (define (required-prop props key)
          (define kv (assoc key props))
          (and kv (cdr kv)))
        (define type (or (required-prop props "edge_label")
                         (required-prop props "type")
                         (error "missing required property:" id key)))
        (define pid (add-predicate type))
        (define (other-key? kv)
          (not (ormap (lambda (k) (equal? k (car kv))) '("type" "edge_label"))))
        ;(define (other-key? kv) (not (equal? "type" (car kv))))
        (set! subject=>edges
          (hash-set subject=>edges subject
                    (cons (edge->bytes
                            (vector pid object-category object id))
                          (hash-ref subject=>edges subject '()))))
        (set! object=>edges
          (hash-set object=>edges object
                    (cons (edge->bytes
                            (vector pid subject-category subject id))
                          (hash-ref object=>edges object '()))))
        (detail-write out-edges out-offset-edges
                      (vector subject pid object (filter other-key? props)))
        (set! id (+ id 1)))))

  (define edges (csv-records in-edge))
  (define edgeprops (csv-records in-edgeprop))
  (define first-edge (edges 'next))
  (define first-edgeprop (edgeprops 'next))
  (when (not (and first-edge first-edgeprop)) (error "empty edge files"))
  (match-define (list eid0 subject-cui object-cui) first-edge)
  (match-define (list eid key raw-val) first-edgeprop)
  (when (not (equal? eid0 eid)) (error "mismatching edge ids:" eid0 eid))

  (let loop ((count 1) (current-eid eid) (subject-cui subject-cui)
                       (object-cui object-cui))
    (define props
      (let loop-props ((row (edgeprops 'current)))
        (match row
          ((list (? (lambda (eid) (equal? current-eid eid))) key raw-val)
           (cons (cons key (json-simplify raw-val))
                 (loop-props (edgeprops 'next))))
          (_ '()))))
    (add-edge subject-cui object-cui props)
    (when (= 0 (remainder count 100000))
      (printf "Processed ~s edges\n" count)
      (flush))

    (match (cons (edges 'next) (edgeprops 'current))
      ((cons (list eid0 subject-cui object-cui) (list eid _ _))
       (when (not (equal? eid0 eid)) (error "mismatching edge ids:" eid0 eid))
       (loop (+ 1 count) eid subject-cui object-cui))
      ((cons #f #f)
       (printf "Found ~s edges involving ~s predicates\n"
               (- count 1) predicate-count)
       (flush)
       (define max-edges/src 0)
       (for ((src-id (in-range 0 (+ 1 (hash-count cui=>id&cat)))))
            (when (= 0 (remainder src-id 10000))
              (printf "Processed edges for ~s concepts\n" src-id)
              (flush-output out-edges-by-subject)
              (flush-output out-offset-edges-by-subject)
              (flush-output out-edges-by-object)
              (flush-output out-offset-edges-by-object))
            (define (write-edges-by-X X=>edges out out-offset)
              (define edges/src 0)
              (offset-write out-offset (file-position out))
              (for ((edge (sort (hash-ref X=>edges src-id '()) bytes<?)))
                   (set! edges/src (+ edges/src 1))
                   (write-bytes edge out))
              (when (< max-edges/src edges/src)
                (printf "Max edges per source concept seen for ~s: ~s\n"
                        src-id edges/src)
                (set! max-edges/src edges/src)))
            (write-edges-by-X
              subject=>edges out-edges-by-subject out-offset-edges-by-subject)
            (write-edges-by-X
              object=>edges out-edges-by-object out-offset-edges-by-object)))
      (_ (error "malformed row(s):" (edges 'current) (edgeprops 'current))))))

(define (call-with-?-files cw?f paths proc)
  (let loop ((paths paths) (ports '()))
    (if (null? paths) (apply proc (reverse ports))
      (cw?f (car paths)
            (lambda (port) (loop (cdr paths) (cons port ports)))))))
(define (call-with-output-files paths proc)
  (call-with-?-files call-with-output-file paths proc))

(printf "Processing nodes:\n")
(time (call-with-input-file
        (graph-path fnin-nodeprop)
        (lambda (in-nodeprop)
          (validate-header nodeprop-header-expected in-nodeprop)
          (call-with-output-files
            (map graph-path (list fnout-concepts
                                  (fname-offset fnout-concepts)
                                  fnout-categories
                                  fnout-concepts-by-category
                                  (fname-offset fnout-concepts-by-category)))
            (lambda out* (apply process-nodes in-nodeprop out*))))))

(printf "\nProcessing edges:\n")
(time (call-with-input-file
        (graph-path fnin-edge)
        (lambda (in-edge)
          (validate-header edge-header-expected in-edge)
          (call-with-input-file
            (graph-path fnin-edgeprop)
            (lambda (in-edgeprop)
              (validate-header edgeprop-header-expected in-edgeprop)
              (call-with-output-files
                (map graph-path (list fnout-predicates
                                      fnout-edges (fname-offset fnout-edges)
                                      fnout-edges-by-subject
                                      (fname-offset fnout-edges-by-subject)
                                      fnout-edges-by-object
                                      (fname-offset fnout-edges-by-object)))
                (lambda out*
                  (apply process-edges in-edge in-edgeprop out*))))))))
