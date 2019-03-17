#lang racket/base
(provide
  ? help
  graph path
  C P T
  show hide count unmodify
  graph? path? C? P? T? show? hide? count? except?
  except
  inote iref aref untag)
(require "common.rkt" "mk.rkt")

(define help "Use (?) or (? <object>) for more information.")
(define (? . _) "TODO: define ?.")

(define (aref alist key)
  (define kv (assoc key alist))
  (unless kv (error "missing association list key:" key alist))
  (cdr kv))
(define (tagged? tag x)
  (and (vector? x) (= 2 (vector-length x)) (eqv? tag (vector-ref x 0))))
(define (tag t x)  (vector t x))
(define (tagref x) (vector-ref x 0))
(define (untag x)  (vector-ref x 1))

;; Selectors
(define (except x)  (tag 'except x))
(define (except? x) (tagged? 'except x))
;; Modifiers
(define (show x)    (tag 'show  (unmodify x)))
(define (hide x)    (tag 'hide  (unmodify x)))
(define (count x)   (tag 'count (unmodify x)))
(define (show? x)   (tagged? 'show x))
(define (hide? x)   (tagged? 'hide x))
(define (count? x)  (tagged? 'count x))
(define (unmodify x)
  (if (or (show? x) (hide? x) (count? x)) (untag x) x))
;; Graph sets
(define (graph? x)  (tagged? 'graph (unmodify x)))
(define (path? x)   (tagged? 'path  (unmodify x)))
;; Element sets: ((dbname id . data) ...)
(define (C? x)      (tagged? 'concept     (unmodify x))) ;; data = (cui name)
(define (P? x)      (tagged? 'predicate   (unmodify x))) ;; data = name
(define (T? x)      (tagged? 'concep-type (unmodify x))) ;; data = name

(define (inote x)
  (define xs (untag (unmodify x)))
  (map cons (range (length xs)) xs))
(define (iref x . args)
  (define u (unmodify x))
  (define t (tagref u))
  (define xs (untag u))
  (define (iref? x) (and (integer? x) (<= 0 x)))
  (define ixs
    (foldl (lambda (arg ixs)
             (cond ((iref? arg) (cons arg ixs))
                   ((and (list? arg) (andmap iref? arg)) (append arg ixs))
                   (else (error "invalid iref argument:" arg args))))
           '() args))
  (define new
    (reverse (caddr (foldl (lambda (i acc)
                             (define offset    (car acc))
                             (define xs        (cadr acc))
                             (define kept      (caddr acc))
                             (define remaining (drop (- i offset) xs))
                             (list i remaining (cons (car remaining) kept)))
                           (list 0 xs '()) (sort ixs <)))))
  (tag t new))

;; These assumes unique pairing of dbname and id.
(define (element<? a b)
  (or (symbol<? (car a) (car b))
      (and (symbol=? (car a) (car b)) (< (cadr a) (cadr b)))))
(define (element=? a b) (and (symbol=? (car a) (car b)) (= (cadr a) (cadr b))))
(define (element-min e . es)
  (cond ((null? es)             e)
        ((element<? e (car es)) (apply element-min e (cdr es)))
        (else                   (apply element-min es))))
(define (element-max e . es)
  (cond ((null? es)             e)
        ((element<? (car es) e) (apply element-max e (cdr es)))
        (else                   (apply element-max es))))
(define (sort-elements es) (sort es element<?))

(define (union* es*)
  (define xs* (filter-not null? es*))
  (cond ((null? xs*) '())
        (else (define xs0 (map car xs*))
              (define next (apply element-min xs0))
              (cons next (union* (map (lambda (xs)
                                        (if (element=? next (car xs)) (cdr xs)
                                          xs))
                                      xs*))))))
(define (intersection* xs*)
  (cond ((ormap null? xs*) '())
        (else (define xs0 (map car xs*))
              (define next (apply element-max xs0))
              (if (andmap (lambda (x) (element=? x next)) xs0)
                (cons next (intersection* (map cdr xs*)))
                (intersection*
                  (map (lambda (xs) (dropf xs (lambda (x) (element<? x next))))
                       xs*))))))
(define (difference as bs)
  (cond ((or (null? as) (null? bs))    as)
        ((element<? (car bs) (car as)) (difference as       (cdr bs)))
        ((element=? (car as) (car bs)) (difference (cdr as) (cdr bs)))
        (else           (cons (car as) (difference (cdr as) bs)))))
(define (difference* a bs) (difference* a (union* bs)))
(define (union        a b) (union*        (list a b)))
(define (intersection a b) (intersection* (list a b)))

(define (parse ~name* superset?* args)
  (define (element-dbname n) (car n))
  (define acc:empty '#(() () ()))
  (define (acc-supersets acc) (vector-ref acc 0))
  (define (acc-dbnames   acc) (vector-ref acc 1))
  (define (acc-~names    acc) (vector-ref acc 2))
  (define (add* acc i x*) (vector-set! acc i (append x* (vector-ref acc i))))
  (define (add  acc i x)       (add*       acc i (list x)))
  (define (add-superset acc x) (add        acc 0 x))
  (define (add-dbname   acc x) (add        acc 1 x))
  (define (add-~name    acc x) (add        acc 2 x))
  (define (handle* arg* pos neg)
    (for-each (lambda (arg) (handle (unmodify arg) pos neg)) arg*))
  (define (handle arg pos neg)
    (cond ((symbol? arg) (add-dbname pos arg))
          ((string? arg) (add-~name  pos arg))
          ((~? arg)      (handle* (untag arg) neg pos))
          ((ormap (lambda (superset?) (superset? arg)) superset?*)
           (add-superset pos arg))
          (else (error "invalid argument:" arg args))))
  (define pos acc:empty)
  (define neg acc:empty)
  (handle* args pos neg)
  (define dbn:pos (acc-dbnames pos))
  (define dbn:neg (acc-dbnames neg))
  (define (dbfilter es)
    (filter (lambda (e)
              (define dbn (element-dbname e))
              (and (not (member dbn dbn:neg))
                   (or (null? dbn:pos) (member dbn dbn:pos))))
            es))
  (let* ((es (difference* (intersection* (map dbfilter (acc-supersets pos)))
                          (map dbfilter (acc-supersets neg))))
         (es (if (null? (acc-~names pos)) es
               (intersection es (dbfilter (~name* (acc-~names pos))))))
         (es (if (null? (acc-~names neg)) es
               (difference es (dbfilter (~name* (acc-~names neg)))))))
    es))

(define (C . args)
  (define (~name strs) (sort-elements (run* (c) (~name*-concepto strs c))))
  (parse ~name (list C? T?) args))
(define (P . args)
  (define (~name strs)
    (intersection*
      (map (lambda (s) (sort-elements (run* (p) (~predicateo s p)))) strs)))
  (parse ~name (list P?) args))
(define (T . args)
  (define (~name strs)
    (intersection*
      (map (lambda (s) (sort-elements (run* (c) (~categoryo s c)))) strs)))
  (parse ~name (list T?) args))

;; TODO: Fix this.  This isn't quite right because it doesn't reflect sharing.
;; Maybe define paths in terms of graphs of edges with named/shared elements?
;; But path presentation/UI is nice.
(define (edge s p o)
  (unless (C? s) (error "invalid edge subject:"   s))
  (unless (P? p) (error "invalid edge predicate:" p))
  (unless (C? o) (error "invalid edge object:"    o))
  (list s p o))
(define (path . args)
  (define (l>=3? xs) (and (pair? xs) (pair? (cdr xs)) (pair? (cddr xs))))
  (tag 'path
       (let loop ((args args) (edges '()))
         (cond ((null? args) edges)
               ((l>=3? args)
                (define e (edge (car args) (cadr args) (caddr args)))
                (loop (cddr args) (cons e edges)))))))

;; TODO: define this as syntax.
(define (graph element-bindings components)
  )

