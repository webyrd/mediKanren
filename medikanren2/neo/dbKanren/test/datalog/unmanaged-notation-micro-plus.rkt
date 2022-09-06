#lang racket/base
(provide run-stratified var? ==)
(require "micro-plus.rkt" (except-in racket/match ==) racket/set)

(define (atom-vars atom) (list->set (map var-name (filter var? atom))))

(define (parse-term expr)
  (match expr
    ((? symbol?) (var expr))
    (`(quote ,c) c)
    ((cons _ _)  (error "unsupported function call" expr))
    (_           expr)))

(define (parse-atom expr) (cons (car expr) (map parse-term (cdr expr))))

(struct rule (head body+ body-) #:prefab)

(define (parse-rule expr)
  (let ((head (parse-atom (car expr))))
    (let loop ((e* (cdr expr)) (atoms.+ '()))
      (define (finish atoms.-)
        (let ((r (rule head (reverse atoms.+) atoms.-)))
          (let ((vars.+ (apply set-union (set) (map atom-vars (rule-body+ r))))
                (vars.- (apply set-union (set) (map atom-vars (rule-body- r)))))
            (unless (subset? (atom-vars (rule-head r)) vars.+)
              (error "rule head is not range-restricted" expr))
            (unless (subset? vars.- vars.+)
              (error "rule negated body atoms are not range-restricted" expr)))
          r))
      (match e*
        ('()            (finish '()))
        ((cons 'not e*) (finish (map parse-atom e*)))
        ((cons e    e*) (loop e* (cons (parse-atom e) atoms.+)))))))

(define (run-stratified predicate=>proc predicate=>merge e**.rules F*)
  (define (enforce r)
    (match-define (rule head atoms.+ atoms.-) r)
    (define (+atom->a atom)
      (let ((proc (hash-ref predicate=>proc (car atom) #f)))
        (if proc
          (compute proc (cdr atom))
          (relate atom))))
    (define (-atom->a atom)
      (let ((proc (hash-ref predicate=>proc (car atom) #f)))
        (if proc
          (reject-compute proc (cdr atom))
          (reject-relate atom))))
    (realize head (conj* (append (map +atom->a atoms.+) (map -atom->a atoms.-)))))
  (foldr (lambda (c&p* F*)
           (match c&p*
             (`(run-once        . ,p*) (produce-once* p* predicate=>merge F*))
             (`(run-fixed-point . ,p*) (exhaust*      p* predicate=>merge F*))))
         F*
         (map (lambda (e*.rules)
                (define (build e*) (map enforce (map parse-rule e*)))
                (match e*.rules
                  ((cons (and (or 'run-once 'run-fixed-point) cmd) e*.rules)
                   (cons cmd (build e*.rules)))
                  (_ (cons 'run-fixed-point (build e*.rules)))))
              e**.rules)))
