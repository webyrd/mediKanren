#lang racket/base
(require
  "csv.rkt"
  "repr.rkt"
  racket/match
  )

(define argv (current-command-line-arguments))
(define argv-expected '#(GRAPH_DIR))

(when (not (= (vector-length argv-expected) (vector-length argv)))
  (error 'cmd-line-args (format "expected ~s; given ~s" argv-expected argv)))

(define graph-dir (vector-ref argv 0))
(define (graph-path fname) (expand-user-path (build-path graph-dir fname)))

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
;; TODO: these need block offsetting.
(define fnout-edges-by-subject      "edges-by-subject.detail")
(define fnout-edges-by-object       "edges-by-object.detail")
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
          (if kv (cdr kv) (error "missing required property:" cui key)))
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
  (match-define (list cui key val) first-nodeprop)

  (let loop ((count 1) (current-cui cui))
    (define props
      (let loop-props ((row (nodeprops 'current)))
        (match row
          ((list (? (lambda (cui) (equal? current-cui cui))) key val)
           (cons (cons key val) (loop-props (nodeprops 'next))))
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

;; TODO:
;(printf "\nProcessing edges:\n")
