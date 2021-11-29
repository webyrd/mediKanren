#lang racket/base
(provide enumerator-append
         list->enumerator enumerator->rlist enumerator->list
         s->enumerator enumerator->s
         vector->enumerator unsafe-vector->enumerator enumerator->rvector enumerator->vector
         generator->enumerator
         enumerator/2->enumerator
         enumerator->enumerator/2
         hash->enumerator/2)
(require racket/control racket/unsafe/ops)

(define ((enumerator-append e.0 e.1) yield)
  (e.0 yield)
  (e.1 yield))

(define ((list->enumerator xs) yield) (for-each yield xs))

(define (enumerator->rlist en)
  (define xs '())
  (en (lambda (x) (set! xs (cons x xs))))
  xs)

(define (enumerator->list en)
  (reverse (enumerator->rlist en)))

(define ((s->enumerator s) yield)
  (let loop ((s s))
    (cond ((null? s) (void))
          ((pair? s) (yield (car s)) (loop (cdr s)))
          (else      (loop (s))))))

(define ((enumerator->s en))
  (define tag (make-continuation-prompt-tag))
  (reset-at tag
            (en (lambda (x)
                  (shift-at tag k (cons x (lambda () (k (void)))))))
            '()))

(define (enumerator->rvector en)
  (list->vector (enumerator->rlist en)))

(define (enumerator->vector en)
  (list->vector (enumerator->list en)))

(define (vector->enumerator v (start 0) (end (vector-length v)))
  (define len (min end (vector-length v)))
  (unsafe-vector->enumerator v (min start len) len))

(define ((unsafe-vector->enumerator v (start 0) (end (unsafe-vector*-length v))) yield)
  (let loop ((i start))
    (when (unsafe-fx< i end)
      (yield (unsafe-vector*-ref v i))
      (loop (unsafe-fx+ i 1)))))

(define ((generator->enumerator gen stop?) yield)
  (let loop ()
    (define x (gen))
    (unless (stop? x)
      (yield x)
      (loop))))

;; An enumerator/2 expects its iteratee to take two arguments
(define ((enumerator/2->enumerator en) yield)
  (en (lambda (a b) (yield (cons a b)))))

(define ((enumerator->enumerator/2 en) yield)
  (en (lambda (x&y) (yield (car x&y) (cdr x&y)))))

(define ((hash->enumerator/2 kvs) yield) (hash-for-each kvs yield))
