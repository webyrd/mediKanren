#lang racket/base
(require "../dbk/database.rkt"
         racket/list racket/pretty racket/runtime-path)

(define-runtime-path path.here ".")
(define db (database (build-path path.here "small")))
(auto-empty-trash? #t)

(define (build-list-relation db type tuples)
  (let-values (((insert! finish) (database-relation-builder db type)))
    (time (for-each insert! tuples))
    (time (finish))))

;; (- R R) etc.

(define specs `((main   . ,(map list (range 500)))
                (lower  . ,(map list (range 200)))
                (middle . ,(map list (range 200 300)))
                (upper  . ,(map list (range 300 500)))))

(for-each (lambda (spec)
            (let ((name   (car spec))
                  (tuples (cdr spec)))
              (unless (database-relation-name? db name)
                (let ((R (build-list-relation db '(int) tuples)))
                  (relation-name-set! R name)
                  (relation-attributes-set! R '(value))
                  (database-commit! db)))))
          specs)

(unless (database-relation-name? db 'gone)
  (let ((R.gone   (database-relation-new db '(int)))
        (R.main   (database-relation db 'main))
        (R.lower  (database-relation db 'lower))
        (R.middle (database-relation db 'middle))
        (R.upper  (database-relation db 'upper)))
    (relation-assign! R.gone (R- R.main R.main))
    ;(relation-assign! R.gone (R+ R.upper R.lower))
    ;(relation-assign! R.gone (R- (R- R.main R.lower) R.upper))
    ;(relation-assign! R.gone (R- R.main (R+ R.lower R.upper)))
    ;(relation-assign! R.gone (R- (R- (R- R.main R.lower) R.upper) R.middle))
    ;(relation-assign! R.gone (R- (R- R.main (R+ R.lower R.upper)) R.middle))
    (relation-name-set! R.gone 'gone)
    (database-commit! db)))

;(relation-full-compact! (database-relation db 'gone))
(database-commit! db)
