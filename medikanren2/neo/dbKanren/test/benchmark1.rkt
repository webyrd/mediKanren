#lang racket/base
(require "../dbk.rkt" racket/function racket/list racket/pretty)
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(define-syntax-rule (test name e expected)
  (begin (printf "Testing ~s:\n" name)
         (pretty-print 'e)
         (let ((answer (time e)))
           (unless (equal? answer expected)
             (printf "FAILED ~s:\n" name)
             (printf "  ANSWER:\n")
             (pretty-print answer)
             (printf "  EXPECTED:\n")
             (pretty-print expected)))))

(for-each (lambda (key) (current-config-set! key 'always))
          '(update-policy cleanup-policy migrate-policy))


;; Benchmark reference: http://users.informatik.uni-halle.de/~brass/botup/

(define-relation/table Q
  'path            "benchmark1/Q"
  'source-stream   (value/syntax
                     (map (lambda (i) (list i (+ i 1))) (range 1 5001)))
  'attribute-names '(a b)
  'attribute-types '(nat nat)
  ;; optionally preload into memory
  'retrieval-type  'scm
  )

(define-relation/table R
  'path            "benchmark1/R"
  'source-stream   (value/syntax
                     (map (lambda (i) (list i    i))    (range 1 5001)))
  'attribute-names '(a b)
  'attribute-types '(nat nat)
  ;; optionally preload into memory
  'retrieval-type  'scm
  )

(define-relation (P3.1 b y) (fresh (x) (P2.1 b x) (Q x y)))
(define-relation (P2.1 c w) (fresh (v) (P1.1 c v) (Q v w)))
(define-relation (P1.1 d u) (fresh (t) (P0.1 d t) (Q t u)))
(define-relation (P0.1 e s) (R e s))

;; disk: ~8000ms
;; scm:  ~1400ms
(test 'benchmark-1
  (length (run* (a z) (P3.1 a z)))
  ;; smallest: (1    4)
  ;; largest:  (4998 5001)
  4998)

(define-relation (P3.2 x z) (fresh (y) (Q x y) (P2.2 y z)))
(define-relation (P2.2 x z) (fresh (y) (Q x y) (P1.2 y z)))
(define-relation (P1.2 x z) (fresh (y) (Q x y) (P0.2 y z)))
(define-relation (P0.2 x y) (R x y))

;; disk: ~8000ms
;; scm:  ~1400ms
(test 'benchmark-2
  (length (run* (x z) (P3.2 x z)))
  ;; smallest: (1    4)
  ;; largest:  (4997 5000)
  4997)


;; TODO: improve performance

;(define-relation/table Q.large
;  'path            "benchmark1/Q.large"
;  'source-stream   (value/syntax
;                     (map (lambda (i) (list i (+ i 1))) (range 1 1000001)))
;  'attribute-names '(a b)
;  'attribute-types '(nat nat)
;  ;; optionally preload into memory
;  'retrieval-type  'scm
;  )
;
;(define-relation/table R.large
;  'path            "benchmark1/R.large"
;  'source-stream   (value/syntax
;                     (map (lambda (i) (list i    i))    (range 1 1000001)))
;  'attribute-names '(a b)
;  'attribute-types '(nat nat)
;  ;; optionally preload into memory
;  'retrieval-type  'scm
;  )
;
;(define-relation (P3.2.large x z) (fresh (y) (Q.large x y) (P2.2.large y z)))
;(define-relation (P2.2.large x z) (fresh (y) (Q.large x y) (P1.2.large y z)))
;(define-relation (P1.2.large x z) (fresh (y) (Q.large x y) (P0.2.large y z)))
;(define-relation (P0.2.large x y) (R.large x y))
;
;;; disk: ?
;;; scm:  ?
;(test 'benchmark-2-large
;  (length (run* (x z) (P3.2.large x z)))
;  '?)


;; TODO: improve performance

;(define-relation/table S
;  'path            "benchmark1/S"
;  'source-stream   (value/syntax (map list (range 1 50001)))
;  'attribute-names '(a)
;  'attribute-types '(nat)
;  ;; optionally preload into memory
;  'retrieval-type  'scm
;  )

;(define-relation (P1.3 a)             (S a))
;(define-relation (P2.3 a b)           (P1.3 a)           (membero b '(1 2)))
;(define-relation (P3.3 a b c)         (P2.3 a b)         (membero c '(1 2)))
;(define-relation (P4.3 a b c d)       (P3.3 a b c)       (membero d '(1 2)))
;(define-relation (P5.3 a b c d e)     (P4.3 a b c d)     (membero e '(1 2)))
;(define-relation (P6.3 a b c d e f)   (P5.3 a b c d e)   (membero f '(1 2)))
;(define-relation (P7.3 a b c d e f g) (P6.3 a b c d e f) (membero g '(1 2)))

;;; disk: ?
;;; scm:  ?
;(test 'benchmark-3
;  (length (run* (a b c d e f g) (P7.3 a b c d e f g)))
;  (* 50000 (expt 2 6)))


;; TODO: fixed-point computations

;(define-relation/table edge
;  'path            "benchmark1/edge"
;  'source-stream   (value/syntax
;                     (map (lambda (i) (list i (+ i 1))) (range 1 1001)))
;  'attribute-names '(a b)
;  'attribute-types '(nat nat)
;  ;; optionally preload into memory
;  'retrieval-type  'scm
;  )
;
;(define-relation (path x z)
;  (conde ((edge x z))
;         ((fresh (y) (edge x y) (path y z)))))
;
;;; disk: ?
;;; scm:  ?
;(test 'benchmark-fixed-point
;  (length (run* (a b) (path a b)))
;  (/ (* 1000 (+ 1000 1)) 2))
