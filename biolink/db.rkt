#lang racket/base
(provide
  make-db

  db:category*
  db:predicate*

  db:catid->category
  db:pid->predicate
  db:catid->cid*
  db:cid->concept
  db:eid->edge
  db:cid&concept-stream
  db:eid&edge/props-stream
  db:subject->edge-stream
  db:object->edge-stream

  db:~category->catid&category*
  db:~predicate->pid&predicate*
  db:~name->cid&concept*
  db:~cui->cid&concept*
  )

(require
  "repr.rkt"
  racket/stream
  racket/string
  )

(define fnin-concepts             "concepts.scm")
(define fnin-categories           "categories.scm")
(define fnin-concepts-by-category "concepts-by-category.scm")
(define fnin-edges                "edges.scm")
(define fnin-predicates           "predicates.scm")
(define fnin-edges-by-subject     "edges-by-subject.bytes")
(define fnin-edges-by-object      "edges-by-object.bytes")
(define (fname-offset fname) (string-append fname ".offset"))

(define (read-all in)
  (define datum (read in))
  (if (eof-object? datum) '() (cons datum (read-all in))))
(define (read-all-from-file path) (call-with-input-file path read-all))

(define (make-db db-dir)
  (define (db-path fname) (expand-user-path (build-path db-dir fname)))
  (define (open-db-path fname) (open-input-file (db-path fname)))
  (define in-concepts-by-category
    (open-db-path fnin-concepts-by-category))
  (define in-offset-concepts-by-category
    (open-db-path (fname-offset fnin-concepts-by-category)))
  (define in-concepts         (open-db-path fnin-concepts))
  (define in-offset-concepts  (open-db-path (fname-offset fnin-concepts)))
  (define in-edges            (open-db-path fnin-edges))
  (define in-offset-edges     (open-db-path (fname-offset fnin-edges)))
  (define in-edges-by-subject (open-db-path fnin-edges-by-subject))
  (define in-edges-by-object  (open-db-path fnin-edges-by-object))
  (define in-offset-edges-by-subject
    (open-db-path (fname-offset fnin-edges-by-subject)))
  (define in-offset-edges-by-object
    (open-db-path (fname-offset fnin-edges-by-object)))

  (define category*
    (list->vector (read-all-from-file (db-path fnin-categories))))
  (define predicate*
    (list->vector (read-all-from-file (db-path fnin-predicates))))
  (define (catid->cid* catid)
    (detail-ref in-concepts-by-category in-offset-concepts-by-category catid))
  (define (cid->concept cid) (detail-ref in-concepts in-offset-concepts cid))
  (define (eid->edge eid)    (detail-ref in-edges in-offset-edges eid))
  (define (cid&concept-stream)    (stream-offset&values in-concepts))
  (define (eid&edge/props-stream) (stream-offset&values in-edges))
  (define (subject->edge-stream cid pid? cat? dst?)
    (stream-edges-by-X in-edges-by-subject in-offset-edges-by-subject
                       cid pid? cat? dst?))
  (define (object->edge-stream cid pid? cat? dst?)
    (stream-edges-by-X in-edges-by-object in-offset-edges-by-object
                       cid pid? cat? dst?))

  (vector category* predicate* catid->cid* cid->concept eid->edge
          cid&concept-stream eid&edge/props-stream
          subject->edge-stream object->edge-stream))

(define (db:category*             db)        (vector-ref db 0))
(define (db:predicate*            db)        (vector-ref db 1))
(define (db:catid->cid*           db . args) (apply (vector-ref db 2) args))
(define (db:cid->concept          db . args) (apply (vector-ref db 3) args))
(define (db:eid->edge             db . args) (apply (vector-ref db 4) args))
(define (db:cid&concept-stream    db)        ((vector-ref db 5)))
(define (db:eid&edge/props-stream db)        ((vector-ref db 6)))
(define (db:subject->edge-stream  db . args) (apply (vector-ref db 7) args))
(define (db:object->edge-stream   db . args) (apply (vector-ref db 8) args))

(define (db:catid->category db catid) (vector-ref (db:category* db) catid))
(define (db:pid->predicate db pid)    (vector-ref (db:predicate* db) pid))

(define (~string->offset* value* value)
  (define needle (string-downcase value))
  (define (p? v) (string-contains? (string-downcase v) needle))
  (let loop ((i (- (vector-length value*) 1)) (o* '()))
    (if (< i 0) o*
      (let ((v (vector-ref value* i)))
        (loop (- i 1) (if (p? v) (cons (cons i v) o*) o*))))))

(define (db:~category->catid&category* db ~category)
  (~string->offset* (db:category* db) ~category))
(define (db:~predicate->pid&predicate* db ~predicate)
  (~string->offset* (db:predicate* db) ~predicate))

(define (~string->offset&value* offset&value* value v->str)
  (define needle (string-downcase value))
  (define (p? v)
    (define haystack (v->str (cdr v)))
    (and haystack (string-contains? (string-downcase haystack) needle)))
  (stream-filter p? offset&value*))

(define (db:~name->cid&concept* db ~name)
  (~string->offset&value* (db:cid&concept-stream db) ~name concept-name))
(define (db:~cui->cid&concept* db ~cui)
  (~string->offset&value* (db:cid&concept-stream db) ~cui concept-cui))
