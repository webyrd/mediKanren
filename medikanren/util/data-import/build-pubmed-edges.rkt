#lang racket/base
(require
  "../../common.rkt"
  "../../repr.rkt"
  (except-in racket/match ==)
  racket/stream)

;; Script to index edges of a mediKanren 1 knowledge graph (KG) by PUBMED identifier.
;;
;;
;; Command-line usage:
;;
;; racket build-pubmed-edges.rkt <path to mediKanren 1 data directory> <name of KG directory within the data directory>
;;
;;
;; Example usage, on Will's laptop, for indexing rtx2_2021_02_04:
;;
;; racket build-pubmed-edges.rkt /Users/webyrd/github/mediKanren/medikanren/data rtx2_2021_02_04


;; The script generates two files, 'edges.scm.offset' and 'edges.scm', in the directory of the KG being indexed.
;;
;; Once these files have been created, you can start Racket and load
;; mediKanren 1 as usual.  Please make sure to require "common.rkt".
;;
;; To get a list of edges that contain the PUBMED id "10022988" (for example), you can use the expression:
;;
;; (run* (e) (pmid-edgeo "10022988" e)


(define argv (current-command-line-arguments))
(define argv-expected '#(DATA_DIR GRAPH_DIR))

(when (not (= (vector-length argv-expected) (vector-length argv)))
  (error "command line argument mismatch:" argv-expected argv))

(define data-dir (vector-ref argv 0))
(define graph-dir (vector-ref argv 1))
(define (graph-path fname)
  (expand-user-path (build-path data-dir graph-dir fname)))

(define (call-with-?-files cw?f paths proc)
  (let loop ((paths paths) (ports '()))
    (if (null? paths) (apply proc (reverse ports))
      (cw?f (car paths)
            (lambda (port) (loop (cdr paths) (cons port ports)))))))
(define (call-with-output-files paths proc)
  (call-with-?-files call-with-output-file paths proc))

(define (fname-offset fname) (string-append fname ".offset"))
(define fnin-edges         "edges.scm")
(define fnout-pubmed-edges "pubmed-edges.scm")

(define (process-pmids in-edges out-pubmed-edges out-offset-pubmed-edges)
  (define pmid=>eid* (hash))
  (let loop ((eid&edge-stream (port->stream-offset&values in-edges))
             (edges-seen 0))
    (cond ((stream-empty? eid&edge-stream)
           (printf "Found total ~s pmids\n" (hash-count pmid=>eid*))
           (for ((pmid (in-list (sort (hash-keys pmid=>eid*) string<?))))
                (define eid* (hash-ref pmid=>eid* pmid))
                (detail-write out-pubmed-edges out-offset-pubmed-edges
                              (cons pmid eid*)))
           (flush-output out-pubmed-edges)
           (flush-output out-offset-pubmed-edges))
          (else
           (define eid&edge (stream-first eid&edge-stream))
           (define eid (car eid&edge))
           (define props (edge/props-props (cdr eid&edge)))
           (for-each
             (lambda (pmid)
               (set! pmid=>eid*
                     (hash-set pmid=>eid* pmid
                               (cons eid (hash-ref pmid=>eid* pmid '())))))
             (pubmed-ids-from-edge-props props))

           (when (= (modulo edges-seen 1000000) 0)
             (printf "seen ~s edges\n" edges-seen)
             (printf "found ~s pmids so far\n" (hash-count pmid=>eid*)))

           (loop (stream-rest eid&edge-stream)
                 (add1 edges-seen))))))

(printf "\nMapping pmids to edges:\n")
(time (call-with-input-file
        (graph-path fnin-edges)
        (lambda (in-edges)
          (call-with-output-files
            (map graph-path (list fnout-pubmed-edges
                                  (fname-offset fnout-pubmed-edges)))
            (lambda out* (apply process-pmids in-edges out*))))))
