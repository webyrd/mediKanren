#lang racket
(provide
  edge-src
  edge-predicate
  edge-dst-category
  edge-dst
  edge-eid
  edge->bytes
  bytes->edge

  offset-write
  offset-count
  offset-ref
  detail-find
  detail-next
  detail-write
  )

;; TODO: stream edges-by-X by mask for a particular source concept, e.g.,
;;   #(#f  #f         #f):     get all.
;;   #(#f  dst-cat-id #f):     get all with some destination category, any pid.
;;   #(#f  _          dst-id): get all with some destination, any pid.
;;   #(pid #f         #f):     get all with some pid and any destination.
;;   #(pid dst-cat-id #f):     get all with some pid and destination category.
;;   #(pid _          dst-id): get all with some pid and destination.
;; TODO: stream edges-by-X by pid set, equivalent to a union of pid-only masks.

;; This is the edge representation for edges-by-X.detail, not for edges.scm.
(define (edge-src e)          (vector-ref e 0))
(define (edge-predicate e)    (vector-ref e 1))
(define (edge-dst-category e) (vector-ref e 2))
(define (edge-dst e)          (vector-ref e 3))
(define (edge-eid e)          (vector-ref e 4))

;; 2 * 3-byte concept-id + 1-byte predicate + 1-byte category + 4-byte eid
(define edge-byte-size (+ 3 3 1 1 4))

(define (byte-at offset n) (bitwise-and 255 (arithmetic-shift n offset)))

(define (edge->bytes e)
  (define c0 (edge-src e))
  (define c1 (edge-dst e))
  (define pid (edge-predicate e))
  (define cc1 (edge-dst-category e))
  (define eid (edge-eid e))
  (bytes (byte-at -16 c0) (byte-at -8 c0) (byte-at 0 c0)
         pid cc1
         (byte-at -16 c1) (byte-at -8 c1) (byte-at 0 c1)
         (byte-at -24 eid) (byte-at -16 eid) (byte-at -8 eid) (byte-at 0 eid)))
(define (bytes->edge bs)
  (define (bref pos) (bytes-ref bs pos))
  (define (bref-to pos offset) (arithmetic-shift (bref pos) offset))
  (define (bcid pos)
    (+ (bref-to pos 16) (bref-to (+ 1 pos) 8) (bref-to (+ 2 pos) 0)))
  (vector (bcid 0) (bref 3) (bref 4) (bcid 5)
          (+ (bref-to 8 24) (bref-to 9 16) (bref-to 10 8) (bref-to 11 0))))

(define (write-scm out scm) (fprintf out "~s\n" scm))

(define offset-size 8)
(define (offset->bytes o) (integer->integer-bytes o offset-size #f #f))
(define (bytes->offset b) (integer-bytes->integer b #f #f 0 offset-size))
(define (offset-write out offset) (write-bytes (offset->bytes offset) out))
(define (offset-count in)
  (file-position in eof)
  (/ (file-position in) offset-size))
(define (offset-ref in n)
  (file-position in (* n offset-size))
  (bytes->offset (read-bytes offset-size in)))

(define (detail-write detail-out offset-out detail)
  (when offset-out (offset-write offset-out (file-position detail-out)))
  (write-scm detail-out detail))
(define (detail-ref detail-in offset-in n)
  (file-position detail-in (offset-ref offset-in n))
  (read detail-in))
(define (detail-next detail-in) (read detail-in))

(define (detail-find detail-in offset-in compare)
  (let loop ((start 0) (end (offset-count offset-in)) (best #f))
    (cond ((< start end)
           (define n (+ start (quotient (- end start) 2)))
           (define detail (detail-ref detail-in offset-in n))
           (case (compare detail)
             ((-1) (loop (+ 1 n) end best))
             ((0) (loop start n n))
             ((1) (loop start n best))))
          (best (detail-ref detail-in offset-in best))
          (else eof))))
