#lang racket
(provide
 (all-defined-out))
(require "dbk/mk.rkt")

(define (query-names-from-rel rel)
  (query (id name)
         (rel id "name" name)))

(define (query-names-from-id-rel id rel)
  (query (name)
         (rel id "name" name)))