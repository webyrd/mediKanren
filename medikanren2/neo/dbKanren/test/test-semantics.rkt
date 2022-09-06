#lang racket/base
(require "../dbk/semantics.rkt" racket/pretty)
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

;; References:
;; https://web.archive.org/web/20201012095657/https://sites.google.com/site/williamcushing/notes/howtoconvertfirst-orderlogicintoclausalform
;; https://en.wikipedia.org/wiki/Drinker_paradox

(define program.0 '((define (eq  a b) (relate == (var a) (var b)))
                    (define (eq2 d c) (relate == (var c) (var d)))
                    (query (x) (and (relate eq  (var x) (var x))
                                    (relate eq2 (var x) (var x))))))
(define program.1 '((query (x y) (relate == (app cons (var x) (var y)) (app cons '1 '2)))))

(define program.drinking '((query ()
                                  (all (b)
                                    (imply (relate Bar (var b))
                                           (exist (p)
                                             (and (relate Person (var p))
                                                  (relate In     (var p) (var b))
                                                  (iff (relate Drinking (var p))
                                                       (all (q) (imply (and (relate Person (var q))
                                                                            (relate In     (var q) (var b)))
                                                                       (relate Drinking (var q))))))))))))

(for-each
  (lambda (p)
    (displayln "ORIGINAL:")
    (pretty-write p)
    (displayln "SIMPLIFIED:")
    (pretty-write (simplify-program                                      #f p))
    (displayln "FACTORED:")
    (pretty-write (factor-program                                           p))
    (displayln "SIMPLIFIED and FACTORED:")
    (pretty-write (factor-program (simplify-program                      #f p)))
    (displayln "SIMPLIFIED and FACTORED and POST-SIMPLIFIED:")
    (pretty-write (simplify-program #t (factor-program (simplify-program #f p))))
    )
  (list
    program.0
    program.1
    program.drinking
    ))
