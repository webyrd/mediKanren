#lang racket/base
(require
  "repr.rkt"
  "string-search.rkt"
  racket/list
  racket/stream
  racket/vector)

(define argv (current-command-line-arguments))
(define argv-expected '#(DATA_DIR GRAPH_DIR))

(when (not (= (vector-length argv-expected) (vector-length argv)))
  (error "command line argument mismatch:" argv-expected argv))

(define data-dir (vector-ref argv 0))
(define graph-dir (vector-ref argv 1))
(define (graph-path fname)
  (expand-user-path (build-path data-dir graph-dir fname)))
(define (assert-file-absent! fnout)
  (when (file-exists? (graph-path fnout))
    (error "file already exists:"
           (path->string (simplify-path (graph-path fnout))))))
(define (output/filename fname out->)
  (printf "writing ~s\n" fname)
  (time (call-with-output-file (graph-path fname) out->)))

(define (fname-offset fname) (string-append fname ".offset"))
(define fnin-concepts             "concepts.scm")
(define fnout-xrefs               "xrefs.scm")
(define fnout-concept-xref        "concepts-by-xref.scm")
(define fnout-concept-cui-corpus  "concept-cui-corpus.scm")
(define fnout-concept-cui-index   "concept-cui-index.bytes")
(define fnout-concept-name-corpus "concept-name-corpus.scm")
(define fnout-concept-name-index  "concept-name-index.bytes")
(assert-file-absent! fnout-xrefs)
(assert-file-absent! (fname-offset fnout-xrefs))
(assert-file-absent! fnout-concept-xref)
(assert-file-absent! (fname-offset fnout-concept-xref))
(assert-file-absent! fnout-concept-cui-corpus)
(assert-file-absent! fnout-concept-cui-index)
(assert-file-absent! fnout-concept-name-corpus)
(assert-file-absent! fnout-concept-name-index)

(printf "loading concepts...\n")
(define concept*
  (time (call-with-input-file
          (graph-path fnin-concepts)
          (lambda (in-concepts)
            (list->vector
              (stream->list
                (stream-map cdr (port->stream-offset&values in-concepts))))))))
(printf "loaded ~a concepts\n" (vector-length concept*))

(let ()
  (printf "gathering concept cross-references...\n")
  (define xref=>concepts
    (time (let loop ((i 0) (xref=>concepts (hash)))
            (cond ((< i (vector-length concept*))
                   (loop (+ 1 i)
                         (foldl (lambda (xref c=>cs)
                                  (hash-update c=>cs xref
                                               (lambda (cs) (cons i cs)) '()))
                                xref=>concepts
                                (concept->xrefs (vector-ref concept* i)))))
                  (else xref=>concepts)))))
  (printf "building xref vector...\n")
  (define xrefs (time (for/vector ((key (in-hash-keys xref=>concepts))) key)))
  (printf "found ~a xrefs\n" (vector-length xrefs))
  (printf "sorting xrefs...\n")
  (time (vector-sort! xrefs string<?))
  (output/filename
    fnout-xrefs
    (lambda (out-xrefs)
      (output/filename
        (fname-offset fnout-xrefs)
        (lambda (out-offsets-xrefs)
          (for ((xref xrefs))
               (detail-write out-xrefs out-offsets-xrefs xref))))))
  (printf "mapping xrefs to concepts...\n")
  (output/filename
    fnout-concept-xref
    (lambda (out-concept-xref)
      (output/filename
        (fname-offset fnout-concept-xref)
        (lambda (out-offsets-concept-xref)
          (for ((xref xrefs))
               (detail-write
                 out-concept-xref out-offsets-concept-xref
                 (sort (set->list (list->set (hash-ref xref=>concepts xref)))
                       <))))))))

(let ()
  (printf "building CUI search corpus...\n")
  (define cui-corpus
    (time (vector-map (lambda (c) (or (concept-cui c) "")) concept*)))
  (printf "building CUI search index...\n")
  (define cui-index (time (string:corpus->index cui-corpus)))
  (printf "indexed ~a CUIs\n" (vector-length cui-index))
  (output/filename
    fnout-concept-cui-corpus
    (lambda (out) (for ((s (in-vector cui-corpus))) (write-scm out s))))
  (output/filename
    fnout-concept-cui-index
    (lambda (out) (write-string-keys out cui-index))))

(let ()
  (printf "building name search corpus...\n")
  (define name-corpus
    (time (vector-map (lambda (c) (let ((name (concept-name c)))
                                    (if name (string/searchable name) "")))
                      concept*)))
  (printf "building name search index...\n")
  (define name-index (time (suffix:corpus->index name-corpus)))
  (printf "indexed ~a suffixes\n" (vector-length name-index))
  (output/filename
    fnout-concept-name-corpus
    (lambda (out) (for ((s (in-vector name-corpus))) (write-scm out s))))
  (output/filename
    fnout-concept-name-index
    (lambda (out) (write-suffix-keys out name-index))))
