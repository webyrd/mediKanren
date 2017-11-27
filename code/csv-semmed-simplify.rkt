#lang racket/base
(require
  "csv.rkt"
  "edge.rkt"
  "read.rkt"
  (except-in racket/control set)
  racket/file
  racket/list
  racket/pretty
  racket/set
  racket/string
  )

;; This is the first normalization pass, which generates several files:

;; * publication-reference.scm
;; ** PREDICATION_ID
;; ** SENTENCE_ID
;; ** PMID

;; * concept.scm
;; ** CUI
;; ** NAME
;; ** (list-of #(SEMTYPE NOVELTY))

;; * cui-by-semtype.scm
;; ** (SEMTYPE . (list-of CUI))

;; * edge-by-[subject,object]/
;; ** index.scm
;; *** [SUBJECT,OBJECT]_CUI
;; *** detail.bin file position of edge block for [SUBJECT,OBJECT]
;; ** detail.bin (byte-encoded)
;; *** [SUBJECT,OBJECT]_CUI (3 bytes)
;; *** [OBJECT,SUBJECT]_CUI (3 bytes)
;; *** PREDICATE            (1 byte)
;; *** SUBJECT_SEMTYPE      (1 byte)
;; *** OBJECT_SEMTYPE       (1 byte)
;; *** PREDICATION_ID       (4 bytes)

;; All PREDICATION_ID, SENTENCE_ID, PMID, CUI, NOVELTY fields have been
;; converted directly to numeric values.

;; All PREDICATE and SEMTYPE fields have been mapped to numeric indices
;; reflecting sorted order.

;; These conversions should save space and speed up loading.

(define argv (current-command-line-arguments))
(define argv-expected '#(IN_CSV OUT_DIR))

(when (not (= (vector-length argv-expected) (vector-length argv)))
  (error 'cmd-line-args (format "expected ~s; given ~s" argv-expected argv)))

(define csv-file-name (vector-ref argv 0))
(define out-dir (vector-ref argv 1))

(define (make-directories* paths)
  (for ((path paths))
       (make-directory* (expand-user-path path))))

(define (call-with-?-file-vector cw?f base-path n proc)
  (define vports (make-vector n))
  (let loop ((i (- n 1)))
    (if (> 0 i) (proc vports)
      (cw?f (build-path base-path (number->string i))
            (lambda (port) (vector-set! vports i port) (loop (- i 1)))))))
(define (call-with-output-file-vector base-path n proc)
  (call-with-?-file-vector call-with-output-file base-path n proc))
(define (call-with-input-file-vector base-path n proc)
  (call-with-?-file-vector call-with-input-file base-path n proc))
(define (call-with-?-files cw?f paths proc)
  (let loop ((paths paths) (ports '()))
    (if (null? paths) (apply proc (reverse ports))
      (cw?f (car paths)
            (lambda (port) (loop (cdr paths) (cons port ports)))))))
(define (call-with-output-files paths proc)
  (call-with-?-files call-with-output-file paths proc))

(define edge-by-subject-dir
  (expand-user-path (build-path out-dir "edge-by-subject/partitioned")))
(define edge-by-object-dir
  (expand-user-path (build-path out-dir "edge-by-object/partitioned")))
(make-directories* (list edge-by-subject-dir edge-by-object-dir))

;; Use these to build enumeration mappings.
(define predicate-file-name (build-path out-dir "PREDICATE.scm"))
(define semtype-file-name (build-path out-dir "SEMTYPE.scm"))

(define (strings->string=>nat ss)
  (make-immutable-hash (map cons ss (range (length ss)))))

(define predicate=>id
  (strings->string=>nat (read-all-from-file predicate-file-name)))
(define semtype=>id
  (strings->string=>nat (read-all-from-file semtype-file-name)))

(pretty-print `(predicate=>id ,predicate=>id))
(pretty-print `(semtype=>id ,semtype=>id))

;; Generate these.
(define out-file-paths
  (map (lambda (p) (expand-user-path (build-path out-dir p)))
       '("publication-reference.scm"
         "concept.scm"
         "cui-by-semtype.scm"
         )))

(define out-edge-paths
  (map (lambda (p) (expand-user-path (build-path out-dir p)))
       '("edge-by-subject/index.scm"
         "edge-by-subject/detail.bin"
         "edge-by-object/index.scm"
         "edge-by-object/detail.bin")))

(define set-empty (set))
(define (concept-add concept-by-cui concept)
  (define key (vector-ref concept 0))
  (define subdata0 (hash-ref concept-by-cui key set-empty))
  (define subdata1 (set-add subdata0 concept))
  (if (eq? subdata0 subdata1) concept-by-cui
    (hash-set concept-by-cui key subdata1)))
(define (concept-sty concept) (vector-ref concept 2))
(define (sty-novelty datum) (vector (concept-sty datum) (vector-ref datum 3)))
(define (write-cui-by-semtype out concept-by-cui)
  (define cui-by-semtype
    (for*/fold ((cui-by-semtype (hash)))
               ((kv (in-hash-pairs concept-by-cui))
                (sty (in-list (map concept-sty (set->list (cdr kv))))))
               (hash-set cui-by-semtype sty
                         (cons (car kv) (hash-ref cui-by-semtype sty '())))))
  (for ((sty (in-list (sort (hash-keys cui-by-semtype) <))))
       (fprintf out "~s\n"
                (cons sty (sort (hash-ref cui-by-semtype sty) <)))))

(define edge-cui-buckets 10)
(define (edge-partition-add vout edge)
  (define cui (edge-src edge))
  (define key (cond ((< 1000000 cui) 9)
                    ((<  400000 cui) 8)
                    ((<  200000 cui) 7)
                    ((<   50000 cui) 6)
                    ((<   40000 cui) 5)
                    ((<   30000 cui) 4)
                    ((<   20000 cui) 3)
                    ((<   10000 cui) 2)
                    ((<    5000 cui) 1)
                    (else 0)))
  (write-bytes (edge->bytes edge) (vector-ref vout key)))

;; At least one PUBMED_ID is corrupted: PREDICATION_ID=57715160: "20126278 [3]"
(define (robust-string->number s)
  (or (string->number s) (string->number (car (string-split s)))))

(define (print-pubref/concept/edge
          vebs-out vebo-out out-pubref out-concept out-cui-by-semtype)
  (define (yield record)
    (define pubref (list->vector (map robust-string->number (take record 3))))
    (define (concept xs)  ;; CUI, NAME, SEMTYPE, NOVELTY
      (define cui (string->number (substring (car xs) 1)))
      (define semtype (hash-ref semtype=>id (caddr xs)))
      (vector cui (cadr xs) semtype (string->number (cadddr xs))))
    (define concepts (drop record 4))
    (define subject (concept (take concepts 4)))
    (define object (concept (drop concepts 4)))
    (define edge (vector (vector-ref subject 0)
                         (vector-ref object 0)
                         (hash-ref predicate=>id (car (drop record 3)))
                         (vector-ref subject 2)
                         (vector-ref object 2)
                         (vector-ref pubref 2)))
    (shift k (cons (list pubref subject object edge) k)))
  (lambda (in)
    (read-line in 'any) ;; Skip header.
    (let loop ((count 1)
               (concept-by-cui (hash))
               (next (reset (and ((csv-records yield) in) #f))))
      (when (= 0 (remainder count 100000))
        (printf "processed ~s rows\n" count)
        (flush-output out-pubref))
      (if next
        (let* ((data (car next))
               (pubref (list-ref data 0))
               (subject (list-ref data 1))
               (object (list-ref data 2))
               (edge (list-ref data 3))
               (k (cdr next)))
          (fprintf out-pubref "~s\n" pubref)
          (edge-partition-add vebs-out edge)
          (edge-partition-add vebo-out (edge-reverse edge))
          (loop (+ 1 count)
                (concept-add (concept-add concept-by-cui subject) object)
                (k #t)))
        (begin
          (printf "Found ~s concepts\n" (hash-count concept-by-cui))
          (for ((key (in-list (sort (hash-keys concept-by-cui) <))))
               (define kdata (set->list (hash-ref concept-by-cui key)))
               (define kdata0 (car kdata))
               (fprintf out-concept "~s\n" (vector (vector-ref kdata0 0)
                                                   (vector-ref kdata0 1)
                                                   (map sty-novelty kdata))))
          (flush-output out-concept)
          (write-cui-by-semtype out-cui-by-semtype concept-by-cui)
          (flush-output out-cui-by-semtype))))))

(time (call-with-output-files
        out-file-paths
        (lambda out*
          (call-with-output-file-vector
            edge-by-subject-dir edge-cui-buckets
            (lambda (vebs-out)
              (call-with-output-file-vector
                edge-by-object-dir edge-cui-buckets
                (lambda (vebo-out)
                  (call-with-input-file
                    (expand-user-path csv-file-name)
                    (apply print-pubref/concept/edge
                           vebs-out vebo-out out*)))))))))

(define (edge-add edges edge)
  (define key (edge-src (bytes->edge edge)))
  (hash-set edges key (cons edge (hash-ref edges key '()))))

(define (write-edges offset edges out-index out-detail)
  (for/fold ((offset offset))
            ((key (in-list (sort (hash-keys edges) <))))
       (define es (hash-ref edges key))
       (for ((e (in-list (sort es bytes<?)))) (write-bytes e out-detail))
       (fprintf out-index "~s\n" (cons key offset))
       (+ offset (* edge-byte-size (length es)))))

(define (edges-index out-index out-detail vin)
  (for/fold ((offset 0))
            ((i (range (vector-length vin))) (in (in-vector vin)))
            (define edges (for/fold ((edges (hash)))
                                    ((edge (read-edge-bytes-all/stream in)))
                                    (edge-add edges edge)))
            (printf "Processing ~s edge buckets in partition ~s\n"
                    (hash-count edges) i)
            (write-edges offset edges out-index out-detail))
  (flush-output out-index)
  (flush-output out-detail))

(time (call-with-output-files
        out-edge-paths
        (lambda (out-ebs-index out-ebs-detail out-ebo-index out-ebo-detail)
          (call-with-input-file-vector
            edge-by-subject-dir edge-cui-buckets
            (lambda (vebs-in)
              (call-with-input-file-vector
                edge-by-object-dir edge-cui-buckets
                (lambda (vebo-in)
                  (displayln "Building edges-by-subject")
                  (edges-index out-ebs-index out-ebs-detail vebs-in)
                  (displayln "Building edges-by-object")
                  (edges-index out-ebo-index out-ebo-detail vebo-in))))))))
