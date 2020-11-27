#lang racket/base
(provide term.min term.max term.number.min term.number.max
         term.symbol.min term.symbol.max term.string.min term.string.max
         term.bytes.min term.bytes.max term.pair.min term.pair.max
         term.vector.min term.vector.max
         domain.any domain.null domain.number domain.symbol
         domain.string domain.bytes domain.pair domain.vector domain.boolean
         any-increment any-decrement
         (struct-out interval)
         type->compare compare-term compare-any compare-null compare-boolean
         compare-nat compare-number
         compare-bytes compare-string compare-symbol
         compare-pair compare-list compare-array compare-tuple
         compare-string/pos compare-suffix compare-suffix-string
         compare-><? compare-><=?
         term<?    term<=?
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

;; TODO: is this representation compatible with table intersections?
;; define polymorphic intersection, likely by lifting to simple OO

;; open intervals for describing infinite domains within the total order
(struct interval (lb ub) #:prefab)

(define term.min        '())
(define term.max        #t)
(define term.number.min term.min)
(define term.number.max '||)
(define term.symbol.min term.number.max)  ;; inclusive
(define term.symbol.max "")
(define term.string.min term.symbol.max)  ;; inclusive
(define term.string.max #"")
(define term.bytes.min  term.string.max)  ;; inclusive
(define term.bytes.max  '(() . ()))
(define term.pair.min   term.bytes.max)   ;; inclusive
(define term.pair.max   '(#t . #t))       ;; inclusive
(define term.vector.min '#())             ;; inclusive
(define term.vector.max #f)

(define domain.any     `#(()       ,(interval '()        #t)))
(define domain.null    `#(()))
(define domain.number  `#(         ,(interval '()        '||)))
(define domain.symbol  `#(||       ,(interval '||        "")))
(define domain.string  `#(""       ,(interval ""         #"")))
(define domain.bytes   `#(#""      ,(interval #""        '(() . ()))))
(define domain.pair    `#((() .()) ,(interval '(() . ()) '(#t . #t)) '(#t . #t)))
(define domain.vector  `#(#()      ,(interval '#()       #f)))
(define domain.boolean `#(#f                             #t))

(define (any-increment x)
  (define (list-increment xs)
    (define xs.new
      (let loop ((xs xs))
        (if (null? xs) '()
          (let* ((a (car xs)) (d (cdr xs)) (d.new (loop (cdr xs))))
            (cond ((and (pair? d.new) (eq? d d.new)) xs)
                  ((andmap null? d.new)
                   (define a.new (if (eq? a #t) '() (any-increment a)))
                   (if (eq? a a.new) xs (cons a.new d.new)))
                  (else (cons a d.new)))))))
    (cond ((null? xs.new)        '(()))
          ((eq? xs xs.new)       xs)
          ((andmap null? xs.new) (cons '() xs.new))
          (else                  xs.new)))
  (match x
    (#f          #t)
    ('(#t . #t)  '#())
    (`(,a . #t)  (define a.new (any-increment a))
                 (if (eq? a a.new) x `(,a.new . ())))
    (`(,a . ,d)  (define d.new (any-increment d))
                 (if (eq? d d.new) x `(,a . ,d.new)))
    ((? vector?) (define xs (vector->list x))
                 (define xs.new (list-increment xs))
                 (if (eq? xs xs.new) x (list->vector xs.new)))
    (_           x)))

(define (any-decrement x)
  (define (list-decrement xs)
    (define xs.new
      (let loop ((xs xs))
        (if (null? xs) '()
          (let* ((a (car xs)) (d (cdr xs)) (d.new (loop (cdr xs))))
            (cond ((and (pair? d.new) (eq? d d.new)) xs)
                  ((andmap (lambda (x) (eq? x #t)) d.new)
                   (define a.new (if (eq? a '()) #t (any-decrement a)))
                   (if (eq? a a.new) xs (cons a.new d.new)))
                  (else (cons a d.new)))))))
    (cond ((eq? xs xs.new)                         xs)
          ((andmap (lambda (x) (eq? x #t)) xs.new) (cdr xs.new))
          (else                                    xs.new)))
  (match x
    (#t          #f)
    ('#()        '(#t . #t))
    (`(,a . ())  (define a.new (any-decrement a))
                 (if (eq? a a.new) x `(,a.new . #t)))
    (`(,a . ,d)  (define d.new (any-decrement d))
                 (if (eq? d d.new) x `(,a . ,d.new)))
    ((? vector?) (define xs (vector->list x))
                 (define xs.new (list-decrement xs))
                 (if (eq? xs xs.new) x (list->vector xs.new)))
    (_           x)))

(define ((compare-><?  compare) a b)      (eqv? (compare a b) -1))
(define ((compare-><=? compare) a b) (not (eqv? (compare a b)  1)))

(define (compare-any a b) (compare/compares compares.any a b))
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

(define (compare-number a b)
  (cond ((< a b) -1)
        ((< b a)  1)
        (else     0)))
(define compare-nat compare-number)

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

(define (compare-tuple cs)
  (define compares (list->vector cs))
  (lambda (a b)
    (define len (vector-length compares))
    (let loop ((i 0))
      (if (= i len) 0
        (let ((compare (vector-ref compares i))
              (va (vector-ref a i)) (vb (vector-ref b i)))
          (case (compare va vb)
            ((-1) -1)
            (( 1)  1)
            (else (loop (+ i 1)))))))))
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

(define (exact-number? x) (and (number? x) (exact? x)))

(define (compare/compares compares a b)
  (let loop ((i 0))
    (let ((type?        (vector-ref compares i))
          (compare-type (vector-ref compares (+ i 1))))
      (cond ((type? a) (if (type? b) (compare-type a b) -1))
            ((type? b) 1)
            (else      (loop (+ i 2)))))))

(define compares.any
  (vector null?         compare-null
          exact-number? compare-number
          symbol?       compare-symbol
          string?       compare-string
          bytes?        compare-bytes
          pair?         (compare-pair compare-any compare-any)
          vector?       (compare-array compare-any)
          boolean?      compare-boolean))

(define (rvar? x) (match x (`#s(var ,_) #t) (_ #f)))
(define (compare-rvar a b)
  (match-define `#s(var ,id.a) a)
  (match-define `#s(var ,id.b) b)
  (compare-number id.a id.b))

(define (compare-term a b) (compare/compares compares.term a b))
(define (term<?  a b)      (eqv? (compare-term a b) -1))
(define (term<=? a b) (not (eqv? (compare-term a b)  1)))
(define compares.term
  (vector rvar?         compare-rvar
          null?         compare-null
          exact-number? compare-number
          symbol?       compare-symbol
          string?       compare-string
          bytes?        compare-bytes
          pair?         (compare-pair compare-term compare-term)
          vector?       (compare-array compare-term)
          boolean?      compare-boolean))

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
