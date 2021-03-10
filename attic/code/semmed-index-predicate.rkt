#lang racket/base
(require
  "edge.rkt"
  racket/file
  )

(define argv (current-command-line-arguments))
(define argv-expected '#(SEMMED_DIR))

(define semmed-dir (vector-ref argv 0))
(define ebs-detail-file (build-path semmed-dir "edge-by-subject/detail.bin"))
(define ebp-dir (build-path semmed-dir "edge-by-predicate"))
(define index-file (expand-user-path (build-path ebp-dir "index.scm")))
(define detail-file (expand-user-path (build-path ebp-dir "detail.bin")))

(make-directory* (expand-user-path ebp-dir))

(define (ebp-key<? a b)
  (or (< (vector-ref a 0) (vector-ref b 0))
      (and (= (vector-ref a 0) (vector-ref b 0))
           (or (< (vector-ref a 1) (vector-ref b 1))
               (and (= (vector-ref a 1) (vector-ref b 1))
                    (< (vector-ref a 2) (vector-ref b 2)))))))

(define (edges-index in-detail out-index out-detail)
  (define ebp (for/fold ((edges (hash)))
                        ((e-bytes (read-edge-bytes-all/stream in-detail)))
                        (define e (bytes->edge e-bytes))
                        (define key (vector (edge-predicate e)
                                            (edge-src-type e)
                                            (edge-dst-type e)))
                        (hash-set edges key
                                  (cons e-bytes (hash-ref edges key '())))))
  (printf "Processing ~s edge buckets\n" (hash-count ebp))
  (for/fold ((offset 0))
            ((key (in-list (sort (hash-keys ebp) ebp-key<?))))
            (define e* (reverse (hash-ref ebp key)))
            (for ((e e*)) (write-bytes e out-detail))
            (fprintf out-index "~s\n" (cons key offset))
            (+ offset (* edge-byte-size (length e*)))))

(define (call-with-?-files cw?f paths proc)
  (let loop ((paths paths) (ports '()))
    (if (null? paths) (apply proc (reverse ports))
      (cw?f (car paths)
            (lambda (port) (loop (cdr paths) (cons port ports)))))))
(define (call-with-output-files paths proc)
  (call-with-?-files call-with-output-file paths proc))

(time (call-with-output-files
        (list index-file detail-file)
        (lambda (out-index out-detail)
          (call-with-input-file
            (expand-user-path ebs-detail-file)
            (lambda (in) (edges-index in out-index out-detail))))))
