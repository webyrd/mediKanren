#lang racket/base
(provide type->compare compare-any compare-null compare-boolean
         compare-nat compare-number
         compare-bytes compare-string compare-symbol
         compare-pair compare-list compare-array compare-tuple
         compare-string/pos compare-suffix compare-suffix-string
         compare-><? compare-><=?
         any<?     any<=?
         null<?    null<=?
         boolean<? boolean<=?
         number<?  number<=?
         pair<?    pair<=?
         list<?    list<=?
         array<?   array<=?
         tuple<?   tuple<=?
         string/pos<?   string/pos<=?
         suffix<?       suffix<=?
         suffix<string? suffix<=string?)
(require racket/match racket/math)

(define ((compare-><?  compare) a b)      (eqv? (compare a b) -1))
(define ((compare-><=? compare) a b) (not (eqv? (compare a b)  1)))

(define (compare-any a b)
  (let loop ((i 0))
    (let ((type?        (vector-ref compares i))
          (compare-type (vector-ref compares (+ i 1))))
      (cond ((type? a) (if (type? b) (compare-type a b) -1))
            ((type? b) 1)
            (else      (loop (+ i 2)))))))
(define (any<?  a b)      (eqv? (compare-any a b) -1))
(define (any<=? a b) (not (eqv? (compare-any a b)  1)))

(define (compare-null a b) 0)
(define (null<?  a b)     #f)
(define (null<=? a b)     #t)

(define (compare-boolean a b)
  (cond ((eq? a b) 0)
        (a         1)
        (else     -1)))
(define (boolean<?  a b) (and b (not a)))
(define (boolean<=? a b) (or  b (not a)))

(define (compare-nat a b)
  (cond ((< a b) -1)
        ((< b a)  1)
        (else     0)))
(define (compare-number a b)
  (cond ((< a b)     -1)
        ((equal? a b) 0)
        ((= a b) (if (exact? b) 1 -1))
        ((nan? b)    -1)
        (else         1)))
(define (number<?  a b)      (eqv? (compare-number a b) -1))
(define (number<=? a b) (not (eqv? (compare-number a b)  1)))

(define (compare-bytes a b)
  (cond ((bytes<? a b) -1)
        ((bytes<? b a)  1)
        (else           0)))
(define (compare-string a b)
  (cond ((string<? a b) -1)
        ((string<? b a)  1)
        (else            0)))
(define (compare-symbol a b)
  (cond ((symbol<? a b) -1)
        ((symbol<? b a)  1)
        (else            0)))

(define ((compare-pair cmp-a cmp-d) a b)
  (let ((aa (car a)) (ba (car b)))
    (case (cmp-a aa ba)
      ((-1) -1)
      (( 1)  1)
      (else (cmp-d (cdr a) (cdr b))))))
(define ((pair<? cmp-a cmp-d) a b)
  (eqv? ((compare-pair cmp-a cmp-d) a b) -1))
(define ((pair<=? cmp-a cmp-d) a b)
  (not (eqv? ((compare-pair cmp-a cmp-d) a b) 1)))

(define ((compare-list compare-element) a b)
  (define (compare a b)
    (cond ((null? a) (if (null? b) 0 -1))
          ((null? b) 1)
          (else      (compare-p a b))))
  (define compare-p (compare-pair compare-element compare))
  (compare a b))
(define ((list<? compare-element) a b)
  (eqv? ((compare-list compare-element) a b) -1))
(define ((list<=? compare-element) a b)
  (not (eqv? ((compare-list compare-element) a b) 1)))

(define ((compare-array compare-element) a b)
  (let ((alen (vector-length a)) (blen (vector-length a)))
    (cond ((< alen blen) -1)
          ((< blen alen)  1)
          (else (let loop ((i 0))
                  (if (= i alen) 0
                    (let ((va (vector-ref a i)) (vb (vector-ref b i)))
                      (case (compare-element va vb)
                        ((-1) -1)
                        (( 1)  1)
                        (else (loop (+ i 1)))))))))))
(define ((array<? compare-element) a b)
  (eqv? ((compare-array compare-element) a b) -1))
(define ((array<=? compare-element) a b)
  (not (eqv? ((compare-array compare-element) a b) 1)))

(define ((compare-tuple compares) a b)
  (define len (vector-length compares))
  (let loop ((i 0))
    (if (= i len) 0
      (let ((compare (vector-ref compares i))
            (va (vector-ref a i)) (vb (vector-ref b i)))
        (case (compare va vb)
          ((-1) -1)
          (( 1)  1)
          (else (loop (+ i 1))))))))
(define ((tuple<? compares) a b)
  (eqv? ((compare-tuple compares) a b) -1))
(define ((tuple<=? compares) a b)
  (not (eqv? ((compare-tuple compares) a b) 1)))

(define (compare-string/pos sa ai sb bi)
  (define alen (string-length sa))
  (define blen (string-length sb))
  (let loop ((ai ai) (bi bi))
    (cond ((= alen ai) (if (= blen bi) 0 -1))
          ((= blen bi) 1)
          (else (let ((ca (string-ref sa ai)) (cb (string-ref sb bi)))
                  (cond ((char<? ca cb) -1)
                        ((char<? cb ca)  1)
                        (else (loop (+ ai 1) (+ bi 1)))))))))
(define (string/pos<? sa ai sb bi)
  (eqv? (compare-string/pos sa ai sb bi) -1))
(define (string/pos<=? sa ai sb bi)
  (not (eqv? (compare-string/pos sa ai sb bi) 1)))

(define ((compare-suffix source) a b)
  (compare-string/pos (vector-ref source (car a)) (cdr a)
                      (vector-ref source (car b)) (cdr b)))
(define (((compare-suffix-string source) sb) a)
  (compare-string/pos (vector-ref source (car a)) (cdr a)
                      sb 0))

(define ((suffix<? source) a b)
  (eqv? ((compare-suffix source) a b) -1))
(define ((suffix<=? source) a b)
  (not (eqv? ((compare-suffix source) a b) 1)))
(define (((suffix<string? source) sb) a)
  (eqv? (((compare-suffix-string source) sb) a) -1))
(define (((suffix<=string? source) sb) a)
  (not (eqv? (((compare-suffix-string source) sb) a) 1)))

(define compares (vector null?    compare-null
                         boolean? compare-boolean
                         number?  compare-number
                         symbol?  compare-symbol
                         string?  compare-string
                         bytes?   compare-bytes
                         pair?    (compare-pair compare-any compare-any)
                         vector?  (compare-array compare-any)))

(define (type->compare type)
  (match type
    (#f                         compare-any)
    ((or 'nat    `#(nat    ,_)) compare-nat)
    ((or 'string `#(string ,_)) compare-string)
    ((or 'symbol `#(symbol ,_)) compare-symbol)
    ((or 'bytes  `#(bytes  ,_)) compare-bytes)
    (`#(suffix ,source)         (compare-suffix source))
    (`#(tuple ,@ts)             (compare-tuple (map type->compare ts)))
    (`(,ta . ,td)               (compare-pair (type->compare ta)
                                              (type->compare td)))
    ('array                     (compare-array compare-any))
    (`#(array ,_ ,t)            (compare-array (type->compare t)))
    ('list                      (compare-list compare-any))
    (`#(list ,_ ,t)             (compare-list (type->compare t)))
    ('number                    compare-number)
    ((or 'true 'false '())      compare-null)))
