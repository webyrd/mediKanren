#lang racket
(require chk)
(require "../../common.rkt")

; test that we can require common.rkt without problems
(chk
    (#:t #t))
