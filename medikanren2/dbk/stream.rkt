#lang racket/base
(provide s-next s-force s-split s-take s-drop s-each s-foldr s-foldl s-scan
         s-append/interleaving s-append*/interleaving
         s-append s-append* s-map/append s-map s-filter s-group s-memo s-lazy
         s-length s-enumerate s-dedup s-limit)
(require racket/function racket/match)

(define (s-next  s) (if (procedure? s)          (s)  s))
(define (s-force s) (if (procedure? s) (s-force (s)) s))

(define (s-limit n s)
  (cond ((or (= n 0) (null? s)) '())
        ((pair? s)              (cons (car s) (s-limit (- n 1) (cdr s))))
        (else                   (thunk (s-limit n (s))))))

(define (s-split n s)
  (match-define (cons rxs s.remaining) (s-foldl n cons '() s))
  (cons (reverse rxs) s.remaining))

(define (s-take n s)
  (if (and n (= n 0)) '()
    (let ((s (s-force s)))
      (if (null? s) '() (cons (car s) (s-take (and n (- n 1)) (cdr s)))))))

(define (s-drop n s) (cdr (s-foldl n (lambda (_ acc) #t) #t s)))

;; TODO: generalize to multiple streams
(define (s-foldr f acc s)
  (cond ((null? s) acc)
        ((pair? s) (f (car s) (s-foldr f acc (cdr s))))
        (else      (thunk (s-foldr f acc (s))))))

(define (s-append*/interleaving s*) (s-foldr s-append/interleaving '() s*))
;; TODO: generalize to multiple streams
(define (s-append/interleaving s1 s2)
  (cond ((null?      s1) (s2))
        ((procedure? s1) (thunk (s-append/interleaving (s2) s1)))
        (else (define d1  (cdr s1))
              (define s1^ (if (procedure? d1) d1 (thunk d1)))
              (cons (car s1) (thunk (s-append/interleaving (s2) s1^))))))

(define (s-append* ss) (s-foldr s-append '() ss))
;; TODO: generalize to multiple streams
(define (s-append a b) (s-foldr cons b a))
(define (s-filter ? s) (s-foldr (lambda (x acc) (if (? x) (cons x acc) acc))
                                '() s))

(define (s-map f s . ss)
  (cond ((null? s) '())
        ((pair? s) (let loop ((ss-pending ss) (rss '()))
                     (if (null? ss-pending)
                       (let ((ss (reverse rss)))
                         (cons (apply f (car s) (map car ss))
                               (apply s-map f (cdr s) (map cdr ss))))
                       (let next ((ss0 (car ss-pending)))
                         (if (procedure? ss0) (thunk (next (ss0)))
                           (loop (cdr ss-pending) (cons ss0 rss)))))))
        (else      (thunk (apply s-map f (s) ss)))))

;; TODO: generalize to multiple streams
(define (s-map/append f s)
  (s-foldr (lambda (x rest) (s-append (f x) rest))
           '() s))

(define (s-each p s) (let ((s (s-force s)))
                       (unless (null? s) (p (car s)) (s-each p (cdr s)))))

(define (s-foldl n f acc s)
  (if (and n (= n 0)) (cons acc s)
    (let ((s (s-force s)))
      (if (null? s) (list acc)
        (s-foldl (and n (- n 1)) f (f (car s) acc) (cdr s))))))

(define (s-scan s acc f)
  (cons acc (cond ((null? s) '())
                  ((pair? s) (s-scan (cdr s) (f (car s) acc) f))
                  (else      (thunk (s-scan (s) acc f))))))

(define (s-length s) (let loop ((s s) (l 0))
                       (cond ((null? s) l)
                             ((procedure? s) (loop (s) l))
                             (else (loop (cdr s) (+ l 1))))))

(define (s-group s ? @)
  (let ((@ (or @ (lambda (x) x))))
    (cond ((null? s)      '())
          ((procedure? s) (thunk (s-group (s) ? @)))
          (else (let next ((x (@ (car s))) (s s))
                  (let loop ((g (list (car s))) (s (cdr s)))
                    (cond ((null? s)      (list g))
                          ((procedure? s) (thunk (loop g (s))))
                          (else (let ((y (@ (car s))))
                                  (if (? y x) (loop (cons (car s) g) (cdr s))
                                    (cons g (next y s))))))))))))

(define (s-memo s)
  (cond ((procedure? s) (let ((v #f) (s s))
                          (thunk (when s (set! v (s-memo (s))) (set! s #f))
                                 v)))
        ((null? s)      '())
        (else           (cons (car s) (s-memo (cdr s))))))

(define (s-lazy s)
  (define (return s)
    (cond ((null? s) '())
          (else      (cons (car s) (s-lazy (cdr s))))))
  (thunk (cond ((procedure? s) (let retry ((s (s)))
                                 (cond ((procedure? s) (thunk (retry (s))))
                                       (else           (return s)))))
               (else           (return s)))))

(define (s-enumerate i s)
  (cond ((null? s) '())
        ((pair? s) (cons (cons i (car s)) (s-enumerate (+ i 1) (cdr s))))
        (else      (thunk                 (s-enumerate i           (s))))))

;; NOTE: only adjacent duplicates are removed
(define (s-dedup s)
  (define (loop x s)
    (cond ((null? s) (list x))
          ((pair? s) (if (equal? x (car s)) (loop x (cdr s))
                       (cons x (loop (car s) (cdr s)))))
          (else      (thunk (loop x (s))))))
  (cond ((null? s) '())
        ((pair? s) (loop (car s) (cdr s)))
        (else      (thunk (s-dedup (s))))))
