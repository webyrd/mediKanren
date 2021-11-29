#lang racket/base
(provide (prefix-out dbk:
                     (combine-out
                       config.default
                       current-config-relation-path
                       current-config-set!/alist))
         (except-out (all-from-out "dbk/dbk.rkt")
                     config.default
                     current-config-relation-path
                     current-config-set!/alist))
(require "dbk/dbk.rkt")
