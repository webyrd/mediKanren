#lang racket/base
(require "equivalence-database.rkt" "../dbk/database.rkt"
         racket/pretty racket/runtime-path)

(define name.equiv-edge         'equivalence-edge)
(define name.equiv-class-member 'equivalence-class-member)

(define-runtime-path path.here ".")

(define db.equiv
  (build-equivalence-database
    (build-path path.here "test-equivalence.db")
    '(
      (#"A" #"B")
      (#"C" #"D")
      (#"C" #"H")
      (#"B" #"F")
      (#"E" #"G")
      (#"G" #"H")
      (#"L" #"M")
      (#"M" #"O")
      (#"N" #"O")
      (#"F" #"O")
      )))

(define r.equiv-edge         (database-relation db.equiv name.equiv-edge))
(define r.equiv-class-member (database-relation db.equiv name.equiv-class-member))

(define-values (text=>id id=>text) (relation-text-dicts r.equiv-class-member #f))
(define (text->id text) (dict-ref text=>id text (lambda (v) v)
                                  (lambda () (error "invalid text" text))))
(define (id->text id)   (dict-ref id=>text id (lambda (v) v)
                                  (lambda () (error "invalid text id" id))))

(define A=>B=>1        (relation-index-dict r.equiv-edge '(A B) #f))
(define rep=>member=>1 (relation-index-dict r.equiv-class-member '(representative member) #f))

((dict-enumerator A=>B=>1)
 (lambda (id.A B=>1)
   (pretty-write `(A: ,(id->text id.A)))
   ((dict-key-enumerator B=>1)
    (lambda (id.B)
      (pretty-write `(B: ,(id->text id.B)))))
   (newline)))

((dict-enumerator rep=>member=>1)
 (lambda (id.rep member=>1)
   (pretty-write `(representative: ,(id->text id.rep)))
   ((dict-key-enumerator member=>1)
    (lambda (id.member)
      (pretty-write `(member: ,(id->text id.member)))))
   (newline)))
