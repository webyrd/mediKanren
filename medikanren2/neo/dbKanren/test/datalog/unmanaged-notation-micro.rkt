#lang racket/base
(provide run-datalog)
(require "micro.rkt" (except-in racket/match ==))
;(require "micro-with-sets.rkt" (except-in racket/match ==))

;; This example syntax demonstrates how to use the core concepts.  This is only
;; one possible syntax.  For instance, you could also implement a Kanren-style
;; syntax that uses the same core concepts.

;; - Programs are made up of rules and facts:
;;   - Atom: a predicate constant followed by zero or more terms
;;   - Rule: a head atom followed by zero or more body atoms
;;   - Fact: a single atom

;; - Terms in rules may be variables or constants:
;;   - An unquoted symbol is treated as a variable.
;;   - Any quoted value is treated as a constant.
;;   - All other non-pair values are treated as constants.
;;   - Variables cannot appear nested in other terms.

;; - Terms in facts are always unquoted constants, including symbols and pairs.

;; - There are no queries.  There are only rules and facts.
;;   - e.g., (run-datalog rules facts) ==> more-facts

(define (atom-vars atom) (filter var? atom))

(define (rule-safe?! rule)
  (let ((vars.body (apply append (map atom-vars (cdr rule)))))
    (for-each (lambda (var.head) (or (member var.head vars.body)
                                     (error "unsafe rule" rule)))
              (atom-vars (car rule)))))

(define (parse-term expr)
  (match expr
    ((? symbol?) (var expr))
    (`(quote ,c) c)
    ((cons _ _)  (error "unsupported function call" expr))
    (_           expr)))

(define (parse-atom expr) (cons (car expr) (map parse-term (cdr expr))))
(define (parse-rule expr) (let ((rule (map parse-atom expr)))
                            (rule-safe?! rule)
                            rule))

(define (enforce rule) (realize (car rule) (conj* (map relate (cdr rule)))))

(define (run-datalog e*.rules F*)
  (exhaust* (map enforce (map parse-rule e*.rules)) F*))
