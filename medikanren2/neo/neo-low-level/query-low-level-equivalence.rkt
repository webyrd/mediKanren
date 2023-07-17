#lang racket/base

(require "../dbKanren/dbk/database.rkt"
         "../dbKanren/dbk/enumerator.rkt"
         "../dbKanren/dbk/logging.rkt"
         racket/runtime-path
         racket/fixnum
         racket/list
         racket/set)

(provide curie->representative
         curies->synonyms
         curie->synonyms
         build-curies-representative-hash
         add-curies-representative-to-hash)

(define (curie->representative curie)
  (define (helper yield)
    (let* ((id.curie (string->id curie))
           (representative=>1 (dict-get curie=>representative=>1 id.curie)))
      ((dict-key-enumerator representative=>1)
       (lambda (id.representative)
         (let ((representative (id->string id.representative)))
           (yield representative))))))
  (if (null? (curies-in-db (list curie)))
      curie
      (car (enumerator->rlist helper))))

(define (curies->synonyms curie*)
  (define (helper yield)
    (let ((curie*=>1 (string*->id=>1
                      (curies-in-db curie*)
                      #;(filter curie-in-db? curie*))))
      ((merge-join fx< curie*=>1 curie=>representative=>1)
       (lambda (__ ___ representative=>1)
         ((merge-join fx< representative=>1 representative=>curie=>1)
          (lambda (__ ___ curie=>1)
            ((dict-key-enumerator curie=>1)
             (lambda (id.curie)
               (let ((curie (id->string id.curie)))
                 (yield curie))))))))))
  (let ((synonyms (enumerator->rlist helper)))
    (remove-duplicates (append curie* synonyms))))

(define (curie->synonyms curie)
  (curies->synonyms (list curie)))

(define build-curie-representative-hash
      (lambda (hash curie)
        (if (hash-has-key? hash curie)
            hash
            (let* ((synonyms (curie->synonyms curie))
                   (representative (find-smallest-string synonyms)))
              (let loop ((h hash) (s* synonyms))
                (cond
                  ((null? s*) h)
                  (else (loop (hash-set h (car s*) representative) (cdr s*)))))))))

(define build-curies-representative-hash
  (lambda (curie*)
    (let loop ((h (hash)) (c* curie*))
      (cond
        ((null? c*) h)
        (else (loop (build-curie-representative-hash h (car c*)) (cdr c*)))))))

(define add-curies-representative-to-hash
  (lambda (representative-hash curie*)
    (let loop ((h representative-hash) (c* curie*))
      (cond
        ((null? c*) h)
        (else (loop (build-curie-representative-hash h (car c*)) (cdr c*)))))))

(define find-smallest-string
  (lambda (string*)
    (let loop ((s* (cdr string*)) (smallest (car string*)))
      (cond
        ((null? s*) smallest)
        (else
         (if (string<? (car s*) smallest)
             (loop (cdr s*) (car s*))
             (loop (cdr s*) smallest)))))))
    
(define (curie-in-db? curie)
    (dict-ref text=>id
              (string->bytes/utf-8 curie)
              (lambda (id) #t)
              (lambda () #f)))

(define (curies-in-db curie*)
    (define curie-text*=>1
      (let* ((text* (map string->bytes/utf-8 curie*))
             (text* (list->vector (sort (remove-duplicates text*) bytes<?))))
        (dict:ref (lambda (i) (vector-ref text* i)) bytes<?
                  (lambda (_) '()) 0 (vector-length text*))))
    (define (helper yield)
      ((merge-join bytes<? curie-text*=>1 text=>id)
       (lambda (text.curie __ ___)
         (yield (bytes->string/utf-8 text.curie)))))
    (enumerator->rlist helper))                      

(define (dict-get d key)
    (dict-ref d key (lambda (v) v) (lambda () (error "dict-get failed" key))))



(define-runtime-path path.here "../neo-data")
(define db-path-under-parent "equivalence/july_13_2023/equivalence.db")
(pretty-log `(loading relation index dictionaries for db)
              path.here
              db-path-under-parent)
(define name.equiv-class-member 'equivalence-class-member)
(define db.equiv (database (build-path path.here db-path-under-parent)))
(define r.equiv-class-member (database-relation db.equiv name.equiv-class-member))
(define representative=>curie=>1 (relation-index-dict r.equiv-class-member '(representative member) #f))
(define curie=>representative=>1 (relation-index-dict r.equiv-class-member '(member representative) #f))

(define-values (text=>id id=>text) (relation-text-dicts r.equiv-class-member #f))
(define (text->id text) (dict-ref text=>id text (lambda (v) v)
                                  (lambda () (error "invalid text" text))))
(define (id->text id)   (dict-ref id=>text id (lambda (v) v)
                                  (lambda () (error "invalid text id" id))))

(define (string*->id=>1 str*) (bytes*->id=>1 (map string->bytes/utf-8 str*)))
(define (bytes*->id=>1 text*)
    (let* ((text* (sort (set->list (list->set text*)) bytes<?))
           (id*   (list->vector (map text->id text*))))
      (dict:ref (lambda (i) (vector-ref id* i)) fx<
                (lambda (_) '()) 0 (vector-length id*))))

(define (string->id str) (text->id (string->bytes/utf-8 str)))
(define (id->string id)  (bytes->string/utf-8 (id->text id)))
