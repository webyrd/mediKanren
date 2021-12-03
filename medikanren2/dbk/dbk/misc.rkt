#lang racket/base
(provide simple-match simple-match-lambda record
         method-lambda method-choose method-unknown method-except method-only
         foldl/and let*/and define-variant
         plist->alist plist-ref alist->plist alist-ref alist-remove alist-update alist-set
         hash-remove*
         call/files let/files
         map/merge map/append
         min-bits min-bytes min-bytes-power2 int? nat?
         bytes-nat-ref bytes-nat-set! nat->bytes bytes->nat
         sum
         normalize-path)
(require (for-syntax racket/base) racket/fixnum racket/list racket/match racket/set)

(define-syntax simple-match-lambda
  (syntax-rules ()
    ((_ (params body ...) ...) (lambda args
                                 (simple-match args
                                   (params body ...) ...)))))

(define-syntax simple-match
  (syntax-rules ()
    ((_ e) (error "no matching pattern:" e))
    ((_ e (pattern body ...) clauses ...)
     (let* ((x e)
            (k.f (lambda () (simple-match x clauses ...))))
       (simple-match-clause (let () body ...) (k.f) x pattern)))))

(define-syntax simple-match-clause
  (syntax-rules ()
    ((_ k.t k.f x ())          (if (null? x) k.t k.f))
    ((_ k.t k.f x (p.a . p.d)) (if (pair? x)
                                 (let ((x.car (car x))
                                       (x.cdr (cdr x)))
                                   (simple-match-clause
                                     (simple-match-clause k.t k.f x.cdr p.d)
                                     k.f x.car p.a))
                                 k.f))
    ((_ k.t k.f x #(p ...))    (if (vector? x)
                                 (let ((x (vector->list x)))
                                   (simple-match-clause k.t k.f x (p ...)))
                                 k.f))
    ((_ k.t k.f x id)          (let ((id x)) k.t))))

(begin-for-syntax
  (define (syntax/context context stx)
    (datum->syntax context (syntax->datum stx)))
  (define (id/suffix id str)
    (define str.id (symbol->string (syntax->datum id)))
    (datum->syntax id (string->symbol (string-append str.id str))))
  (define (syntax-permute keys stx.alist blame)
    (define kvs
      (syntax-case stx.alist ()
        (((key . value) ...) (map cons
                                  (syntax->datum #'(key ...))
                                  (syntax->list #'((key . value) ...))))))
    (map (lambda (k) (let ((kv (assoc k kvs)))
                       (if kv (cdr kv)
                         (error "missing record field:" k blame))))
         keys))
  (define-syntax with-syntax*
    (syntax-rules ()
      ((_ ()                  body ...) (with-syntax () body ...))
      ((_ ((p0 e0) (p e) ...) body ...) (with-syntax ((p0 e0))
                                          (with-syntax* ((p e) ...)
                                            body ...))))))

;; (record name (field ...) extra ...)
;; produces these definitions:
;; * struct:         (struct name:struct (field ...) extra ...)
;; * predicate:      name?
;; * constructor:    (name (field expr) ...)
;; * setter:         (name:set expr (field expr) ...)
;; * match-expander: (name:match (field pattern) ...)
(define-syntax (record stx)
  (syntax-case stx ()
    ((_ name (field-name ...) extras ...)
     (let* ((field-names     (syntax->datum #'(field-name ...)))
            (field-name-strs (map symbol->string field-names)))
       (with-syntax* ((id.struct  (id/suffix #'name ":struct"))
                      (id.ref     (id/suffix #'name ":ref"))
                      (id.set     (id/suffix #'name ":set"))
                      (id.?       (id/suffix #'name "?"))
                      (id.struct? (id/suffix #'name ":struct?"))
                      (id.match   (id/suffix #'name ":match"))
                      (ids.field-names #'(field-name ...))
                      ((id.accessor ...)
                       (map (lambda (f) (id/suffix #'name
                                                   (string-append "-" f)))
                            field-name-strs))
                      ((id.struct-accessor ...)
                       (map (lambda (f) (id/suffix #'id.struct
                                                   (string-append "-" f)))
                            field-name-strs)))
         #`(begin
             (struct id.struct (field-name ...) extras ...)
             (define id.?        id.struct?)
             (define id.accessor id.struct-accessor) ...
             (...
               (begin
                 (define-syntax (name stx)
                   (syntax-case stx ()
                     ((_ (f e) ...)
                      (with-syntax ((((f e) ...) (syntax-permute
                                                   'ids.field-names
                                                   #'((f e) ...)
                                                   stx)))
                        #'(let ((f e) ...) (id.struct f ...))))))
                 (define-syntax (id.set stx)
                   (syntax-case stx ()
                     ((_ r (f e) ...)
                      (let ((field-names '#,field-names))
                        #`(let ()
                            (match-define (id.struct #,@field-names) r)
                            (let #,(map list
                                        (syntax->datum #'(f ...))
                                        (syntax->list  #'(e ...)))
                              (id.struct #,@field-names)))))))
                 (define-match-expander id.match
                   (lambda (stx)
                     (syntax-case stx ()
                       ((_ (f p) ...)
                        #'(struct* id.struct ((f p) ...))))))))))))))

(define (method-unknown name . args) (error "unknown method:" name args))
(define (method-except m names)
  (lambda (name . args)
    (apply (if (member name names) method-unknown m) name args)))
(define (method-only m names)
  (lambda (name . args)
    (apply (if (member name names) m method-unknown) name args)))

(define-syntax method-choose
  (syntax-rules (else)
    ((_ ((name ...) body ...) ... (else else-body ...))
     (lambda (method-name . args)
       (apply (case method-name
                ((name ...) body ...) ...
                (else       else-body ...))
              method-name args)))
    ((_ body ...) (method-choose body ... (else method-unknown)))))

(define-syntax method-lambda
  (syntax-rules (else)
    ((_ ((name . param) body ...) ... (else else-body ...))
     (method-choose ((name) (lambda (_ . param) body ...)) ... (else else-body ...)))
    ((_ body ...) (method-lambda body ... (else method-unknown)))))

(define (foldl/and f acc xs . yss)
  (let loop ((acc acc) (xs xs) (yss yss))
    (if (null? xs)
      acc
      (and acc (loop (apply f (car xs) (append (map car yss) (list acc)))
                     (cdr xs)
                     (map cdr yss))))))

(define-syntax let*/and
  (syntax-rules ()
    ((_ () body ...)                   (let () body ...))
    ((_ ((lhs rhs) rest ...) body ...) (let ((lhs rhs))
                                         (and lhs (let*/and (rest ...)
                                                    body ...))))))

(define-syntax define-variant
  (syntax-rules ()
    ((_ type? (struct-name fields ...) ...)
     (begin (define (type? x)
              (match x
                ((struct-name fields ...) #t) ...
                (_                        #f)))
            (struct struct-name (fields ...) #:prefab) ...))))

(define (plist->alist plist)
  (match plist
    ('()                     '())
    ((cons k (cons v plist)) (cons (cons k v) (plist->alist plist)))))

(define (plist-ref plist key (default (void)))
  (let loop ((kvs plist))
    (match kvs
      ('()                   (if (void? default)
                               (error "missing key in property list:" key plist)
                               default))
      ((cons k (cons v kvs)) (if (equal? k key)
                               v
                               (loop kvs))))))

(define (alist->plist alist)
  (match alist
    ('()                     '())
    ((cons (cons k v) alist) (cons k (cons v (alist->plist alist))))))

(define (alist-ref alist key (default (void)))
  (define kv (assoc key alist))
  (cond (kv              (cdr kv))
        ((void? default) (error "missing key in association list:" key alist))
        (else            default)))

(define (alist-remove alist key)
  (filter (lambda (kv) (not (equal? (car kv) key))) alist))

(define (alist-update alist key v->v (default (void)))
  (let loop ((kvs alist) (prev '()))
    (cond ((null?        kvs     )
           (when (void? default) (error "missing key in association list:" key alist))
           (cons (cons key (v->v default)) alist))
          ((equal? (caar kvs) key) (foldl cons (cons (cons key (v->v (cdar kvs))) (cdr kvs)) prev))
          (else                    (loop (cdr kvs) (cons (car kvs) prev))))))

(define (alist-set alist key value)
  (alist-update alist key (lambda (_) value) #f))

(define (hash-remove* h keys)
  (foldl (lambda (k h) (hash-remove h k)) h keys))

(define (call/files fins fouts p)
  (let loop ((fins fins) (ins '()))
    (if (null? fins)
      (let loop ((fouts fouts) (outs '()))
        (if (null? fouts)
          (apply p (append (reverse ins) (reverse outs)))
          (call-with-output-file
            (car fouts) (lambda (out) (loop (cdr fouts) (cons out outs))))))
      (call-with-input-file
        (car fins) (lambda (in) (loop (cdr fins) (cons in ins)))))))

(define-syntax-rule (let/files ((in fin) ...) ((out fout) ...) body ...)
  (call/files (list fin ...) (list fout ...)
              (lambda (in ... out ...) body ...)))

(define (map/merge f merge default xs)
  (define ys (reverse (cond ((list?   xs) (map f               xs))
                            ((vector? xs) (map f (vector->list xs)))
                            ((set?    xs) (set-map  xs f))
                            ((hash?   xs) (hash-map xs (lambda (k v) (f (cons k v)))))
                            (else         (error "invalid map/merge collection:" xs)))))
  (if (null? ys)
    default
    (foldl merge (car ys) (cdr ys))))

(define (map/append f xs) (append* (map f xs)))

(define (min-bits n)
  (if (< 0 n)
    (+ 1 (min-bits (fxrshift n 1)))
    0))

(define (min-bytes n)
  (let ((min-bits (min-bits n)))
    (+ (quotient min-bits 8)
       (if (= 0 (remainder min-bits 8)) 0 1))))

(define (min-bytes-power2 n)
  (define c (min-bytes n))
  (cond ((<= c 1) 1)
        ((<= c 2) 2)
        ((<= c 4) 4)
        (else     8)))

(define (int? x) (and (number? x) (exact? x) (integer? x)))
(define (nat? x) (and (int? x) (<= 0 x)))

(define (bytes-nat-set! bs size offset n)
  ;(integer->integer-bytes n size #f #t bs offset) (void)
  (let ((end (+ offset size)))
    (let loop ((i offset) (shift (* 8 (- size 1))))
      (cond ((< i end) (bytes-set! bs i (bitwise-and 255 (fxrshift n shift)))
                       (loop (+ i 1) (- shift 8)))
            (else      bs)))))

(define (bytes-nat-ref bs size offset)
  ;(integer-bytes->integer bs #f #t offset (+ offset size))
  (let ((end (+ offset size)))
    (let loop ((i offset) (n 0))
      (cond ((< i end) (loop (+ i 1) (+ (fxlshift n 8)
                                        (bytes-ref bs i))))
            (else      n)))))

(define (nat->bytes size n)  (let ((bs (make-bytes size 0)))
                               (bytes-nat-set! bs size 0 n)
                               bs))

(define (bytes->nat bs size) (bytes-nat-ref bs size 0))

(define (sum xs) (foldl + 0 xs))

(define (normalize-path path) (path->string (simplify-path (resolve-path (simplify-path path)))))
