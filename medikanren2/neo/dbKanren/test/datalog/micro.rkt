#lang racket/base
(provide
  (struct-out var)
  unit fail
  conj conj+ conj*
  disj disj+ disj*
  == relate
  realize exhaust*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terms and substitution ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct var (name) #:prefab)
(define subst.empty '())

(define (subst-extend S x t)
  (let ((name.x (var-name x)))
    (and (not (occurs? S name.x t))
         (cons (cons name.x t) S))))

(define (walk S t)
  (cond ((var? t) (let ((kv (assoc (var-name t) S)))
                    (if kv (walk S (cdr kv)) t)))
        (else     t)))

(define (walk* S t)
  (cond ((var? t)    (let ((kv (assoc (var-name t) S)))
                       (if kv (walk* S (cdr kv)) t)))
        ((pair? t)   (cons (walk* S (car t)) (walk* S (cdr t))))
        ((vector? t) (list->vector (walk* S (vector->list t))))
        (else        t)))

(define (occurs? S name.x t)
  (let ((t (walk S t)))
    (or (and (var? t) (equal? name.x (var-name t)))
        (and (pair? t) (or (occurs? S name.x (car t)) (occurs? S name.x (cdr t))))
        (and (vector? t) (occurs? S name.x (vector->list t))))))

(define (unify S u v)
  (let ((u (walk S u)) (v (walk S v)))
    (cond ((eqv? u v)  S)
          ((var? u)    (if (and (var? v) (equal? (var-name u) (var-name v)))
                         S
                         (subst-extend S u v)))
          ((var? v)    (subst-extend S v u))
          ((pair? u)   (and (pair? v)
                            (let ((S (unify S (car u) (car v))))
                              (and S
                                   (unify S (cdr u) (cdr v))))))
          ((vector? u) (and (vector? v)
                            (unify S (vector->list u) (vector->list v))))
          (else        (and (equal? u v) S)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Goals, ambitions, producers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Goal     = S -> S*
;; Ambition = F* -> Goal
;; Producer = F* -> F*

(define (bind S* g) (if (null? S*) '() (append (g (car S*)) (bind (cdr S*) g))))

(define unit         (lambda (F*) (lambda (S) (list S))))
(define fail         (lambda (F*) (lambda (S) '())))
(define (== t0 t1)   (lambda (F*) (lambda (S) (let ((S (unify S t0 t1)))
                                                (if S (list S) '())))))
(define (conj a0 a1) (lambda (F*) (let ((g0 (a0 F*)) (g1 (a1 F*)))
                                    (lambda (S) (bind (g0 S) g1)))))
(define (disj a0 a1) (lambda (F*) (let ((g0 (a0 F*)) (g1 (a1 F*)))
                                    (lambda (S) (append (g0 S) (g1 S))))))
(define (conj+ a a*) (if (null? a*) a (conj a (conj+ (car a*) (cdr a*)))))
(define (disj+ a a*) (if (null? a*) a (disj a (disj+ (car a*) (cdr a*)))))
(define (conj* a*)   (if (null? a*) unit (conj+ (car a*) (cdr a*))))
(define (disj* a*)   (if (null? a*) fail (disj+ (car a*) (cdr a*))))

(define (relate atom)
  (lambda (F*)  ; This staging significantly improves performance.
    ((disj* (map (lambda (F) (== atom F))
                 (filter (lambda (F) (unify subst.empty atom F)) F*)))
     'ignored)))

(define (unique-cons x xs) (if (member x xs) xs (cons x xs)))
(define (unique-append xs ys)
  (if (null? xs)
    ys
    (unique-cons (car xs) (unique-append (cdr xs) ys))))

(define remember         (lambda (F*) F*))
(define (realize atom a) (lambda (F*) (map (lambda (S) (walk* S atom))
                                           ((a F*) subst.empty))))
(define (combine p0 p1)  (lambda (F*) (unique-append (p0 F*) (p1 F*))))
(define (combine* p*)    (if (null? p*)
                           remember
                           (combine (car p*) (combine* (cdr p*)))))
(define (exhaust p F*)   (let ((F*.new (p F*)))
                           (if (eq? F* F*.new) F* (exhaust p F*.new))))
(define (exhaust* p* F*) (exhaust (combine* p*) F*))
