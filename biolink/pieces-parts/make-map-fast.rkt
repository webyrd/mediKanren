#lang racket

(require
  racket/runtime-path)

;; Faster version of the `make-map.rkt` functionality, reading
;; directly from the raw data files.

;; *** Change this string to match the name of the KG you want to map! ***
(define kg-name "pr-owl")

(define-runtime-path path:root "..")
(define (path/root relative-path) (build-path path:root relative-path))
(define path:data                 (path/root "data"))
(define (path/data relative-path) (build-path path:data kg-name relative-path))

(define predicates-file-path (path/data "predicates.scm"))
(define concepts-file-path (path/data "concepts.scm"))
(define edges-file-path (path/data "edges.scm"))


(define predicates-vector
  (with-input-from-file
      predicates-file-path
      (lambda ()
        (let loop ((ls '())
                   (x (read)))
          (cond
            ((eof-object? x) (list->vector (reverse ls)))
            (else (loop (cons x ls) (read))))))
      #:mode 'text))

(printf "created predicates vector with ~s entries\n" (vector-length predicates-vector))
;(printf "created predicates vector:\n~s\n\n" predicates-vector)


(define concept/curie-prefix-vector
  (time
   (let ((number-of-concepts (with-input-from-file
                                 concepts-file-path
                               (lambda () (let loop ((i 0) (x (read)))
                                            (cond
                                              [(eof-object? x) i]
                                              [else (loop (add1 i) (read))])))
                               #:mode 'text)))
     (printf "ready to read ~s concepts\n" number-of-concepts)    
     (with-input-from-file
         concepts-file-path
       (lambda ()
         (let ((vec (make-vector number-of-concepts)))
           (printf "created vector to hold ~s concept/curie-prefixes\n" number-of-concepts)
           (let loop ((i 0)
                      (good-curie-count 0)
                      (non-standard-curie-count 0)
                      (x (read)))
             (when (= (modulo i 100000) 0)
               (printf "read \n~s good CURIES and ~s non-standard CURIES so far...\n" good-curie-count non-standard-curie-count))
             (cond
               ((eof-object? x)
                (printf "finished reading all CURIES:\n~s good CURIES and ~s non-standard CURIES\n" good-curie-count non-standard-curie-count)
                vec)
               (else
                (begin
                  (let ((curie (vector-ref x 0)))
                    (cond
                      [(string-contains? curie ":")
                       (let ((curie-prefix (car (string-split curie ":" #:trim? #f))))
                         (begin
                           (vector-set! vec i curie-prefix)
                           (loop (add1 i)
                                 (add1 good-curie-count)
                                 non-standard-curie-count
                                 (read))))]
                      [else
                       (begin
                         (vector-set! vec i "*non-standard CURIE*")
                         (loop (add1 i)
                               good-curie-count
                               (add1 non-standard-curie-count)
                               (read)))]))))))))
       #:mode 'text))))

(printf "created concept/curie-prefix vector with ~s entries\n" (vector-length concept/curie-prefix-vector));(printf "created concept/curie-prefix vector:\n~s\n\n" concept/curie-prefix-vector)


(define edge-type-hash
  (time
   (with-input-from-file
       edges-file-path
     (lambda ()
       (let ((ht (make-hash)))
         (let loop ((i 0)
                    (x (read)))
           (when (= (modulo i 100000) 0)
             (printf "read \n~s edges so far...\n" i)
             ;(printf "ht: ~s\n" ht)
             )
           (cond
             ((eof-object? x) ht)
             (else
              (let ((subject-id (vector-ref x 0))
                    (predicate-id (vector-ref x 1))
                    (object-id (vector-ref x 2)))
                (let ((subject-curie-prefix (vector-ref concept/curie-prefix-vector subject-id))
                      (predicate-string (vector-ref predicates-vector predicate-id))
                      (object-curie-prefix (vector-ref concept/curie-prefix-vector object-id)))
                  (let ((key (list subject-curie-prefix predicate-string object-curie-prefix)))
                    (let ((count (hash-ref ht key #f)))
                      (begin
                        (if count
                            (hash-set! ht key (add1 count))
                            (hash-set! ht key 1))
                        (loop (add1 i) (read))))))))))))
     #:mode 'text)))

(printf "created edge-type hash table with ~s entries\n" (hash-count edge-type-hash))
(printf "edge-type hash table: ~s\n" edge-type-hash)

#|
The entire dot graph can then be copied and pasted into a vis.js file and parsed as a DOT network. 
|#
(define out (open-output-file (string-append kg-name ".dot") #:exists 'replace))
(fprintf out "digraph{graph [ bgcolor=lightgray, fontname=Arial, fontcolor=blue, fontsize=12 ]; node [ fontname=Arial, fontcolor=blue, fontsize=11]; edge [ fontname=Helvetica, fontcolor=red, fontsize=10, labeldistance=2, labelangle=-50 ]; splines=\"FALSE\"; rankdir=\"LR\";")
(map
 (lambda (key)
   (match key
     [`(,subject-curie-prefix ,predicate-string ,object-curie-prefix)
      (let ((count (hash-ref edge-type-hash key)))
        (fprintf out "\t~s -> ~s [label=\"~a (~s)\"]; " subject-curie-prefix object-curie-prefix predicate-string count))]))
 (hash-keys edge-type-hash))
(fprintf out "}")
(close-output-port out)
