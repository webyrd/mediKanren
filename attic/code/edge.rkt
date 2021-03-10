#lang racket/base
(provide
  bytes->edge
  edge->bytes
  edge-byte-size
  edge-dst
  edge-src
  edge-predicate
  edge-src-type
  edge-dst-type
  edge-pub-info
  edge-meaning=?
  edge-pubrefs-set
  edge-reverse
  read-edge-bytes
  read-edge-bytes-all/stream
  )

(require
  "read.rkt"
  racket/stream
  )

;; 2 * 3-byte CUI + 1-byte predicate + 2 * 1-byte semtypes + 4-byte pubref
(define edge-byte-size (+ (* 2 3) 1 2 4))

(define (byte-at offset n) (bitwise-and 255 (arithmetic-shift n offset)))

(define (edge->bytes e)
  (define c0 (vector-ref e 0))
  (define c1 (vector-ref e 1))
  (define pid (vector-ref e 5))
  (bytes (byte-at -16 c0) (byte-at -8 c0) (byte-at 0 c0)
         (byte-at -16 c1) (byte-at -8 c1) (byte-at 0 c1)
         (vector-ref e 2) (vector-ref e 3) (vector-ref e 4)
         (byte-at -24 pid) (byte-at -16 pid) (byte-at -8 pid) (byte-at 0 pid)))

(define (bytes->edge bs)
  (define (bref pos) (bytes-ref bs pos))
  (define (bref-to pos offset) (arithmetic-shift (bref pos) offset))
  (define (bcui pos)
    (+ (bref-to pos 16) (bref-to (+ 1 pos) 8) (bref-to (+ 2 pos) 0)))
  (vector (bcui 0) (bcui 3) (bref 6) (bref 7) (bref 8)
          (+ (bref-to 9 24) (bref-to 10 16) (bref-to 11 8) (bref-to 12 0))))

(define (edge-src edge) (vector-ref edge 0))
(define (edge-dst edge) (vector-ref edge 1))
(define (edge-predicate edge) (vector-ref edge 2))
(define (edge-src-type edge) (vector-ref edge 3))
(define (edge-dst-type edge) (vector-ref edge 4))
(define (edge-pub-info edge) (vector-ref edge 5))
(define (edge-meaning=? e1 e2)
  (and (= (edge-src e1) (edge-src e2))
       (= (edge-dst e1) (edge-dst e2))
       (= (edge-predicate e1) (edge-predicate e2))
       (= (edge-src-type e1) (edge-src-type e2))
       (= (edge-dst-type e1) (edge-dst-type e2))))
(define (edge-pubrefs-set edge pubrefs)
  (vector (edge-src edge)
          (edge-dst edge)
          (edge-predicate edge)
          (edge-src-type edge)
          (edge-dst-type edge)
          pubrefs))

(define (edge-reverse edge)
  (vector (edge-dst edge) (edge-src edge) (edge-predicate edge)
          (edge-dst-type edge) (edge-src-type edge) (edge-pub-info edge)))

(define (read-edge-bytes in) (read-bytes edge-byte-size in))
(define (read-edge-bytes-all/stream in)
  (define ebs (read-edge-bytes in))
  (if (eof-object? ebs) '() (stream-cons ebs (read-edge-bytes-all/stream in))))
