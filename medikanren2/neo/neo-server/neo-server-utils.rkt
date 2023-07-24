#lang racket/base

(provide get-assoc
         list-assoc
         mvp2-1hop-filter
         mvp2-2hop-filter
         auto-grow
         merge-list
         merge-hash
         minus-one-before-zero
         find-max-number
         )

(require racket/list
         racket/math)

(define (merge-list xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (merge-list ys (cdr xs)))))

(define (merge-hash h1 h2)
  (define h h2)
  (hash-for-each h1 (lambda (k v) (set! h (hash-set h k v))))
  h)

(define (get-assoc k m)
  (let ((r (assoc k m)))
    (if r
        (cadr r)
        #f)))

(define (list-assoc k m)
  (let ((r (assoc k m)))
    (if r
        (cdr r)
        '())))

(define mvp2-filter
        (lambda (target-eprop direction)
          (let* ((aspect (or (get-assoc "object_aspect_qualifier" target-eprop)
                             (get-assoc "qualified_object_aspect" target-eprop)
                             ))
                 (direction^ (or (get-assoc "object_direction_qualifier" target-eprop)
                                 (get-assoc "qualified_object_direction" target-eprop)
                                 )))
            (and
             aspect
             direction^
             (or
              (equal? "activity" aspect)
              (equal? "abundance" aspect)
              (equal? "activity_or_abundance" aspect))
             (equal? direction direction^)
             ))))

(define mvp2-2hop-filter
  (lambda (q direction)
    (filter
     (lambda (e)
       (let-values ([(_ eprop) (split-at e 5)])
         (mvp2-filter (cadr eprop) direction)))
     q)))

(define mvp2-1hop-filter
  (lambda (q direction)
    (filter
     (lambda (e)
       (let-values ([(_ eprop) (split-at e 3)])
         (mvp2-filter eprop direction)))
     q)))

(define minus-one-before-zero
  (lambda (n*)
    (and n*
        (if (eq? (car n*) 1)
            #f
            (list (- (car n*) 1))))))

(define (auto-grow hop-proc score* unique_results_amount)
  (let ((half-result (exact-round (/ unique_results_amount 2.0))))
    (let loop ((r '()) (sl score*))
      (cond
        [(> (length r) half-result)
         (printf "current length of result: ~a\n" (length r))
         r]
        [(andmap not sl) r]
        [else (loop (append r (hop-proc sl))
                    (list (minus-one-before-zero (list-ref sl 0))
                          (minus-one-before-zero (list-ref sl 1))
                          (minus-one-before-zero (list-ref sl 2))))]))))

(define find-max-number
  (lambda (num*)
    (let loop ((n* (cdr num*)) (greatest (car num*)))
      (cond
        ((null? n*) greatest)
        (else
         (if (> (car n*) greatest)
             (loop (cdr n*) (car n*))
             (loop (cdr n*) greatest)))))))
