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
  db:pmid->eid*
  db:pmid&eid*-stream

  db:~category->catid&category*
  db:~predicate->pid&predicate*
  db:~name->cid&concept*
  db:~cui->cid&concept*
  db:~name*->cid&concept*/options

  chars:ignore-typical
  chars:split-typical
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
(define fnin-pubmed-edges         "pubmed-edges.scm")
(define (fname-offset fname) (string-append fname ".offset"))

(define (read-all in)
  (define datum (read in))
  (if (eof-object? datum) '() (cons datum (read-all in))))
(define (read-all-from-file path) (call-with-input-file path read-all))

;; TODO: incorporate a time/space tradeoff parameter.
;; memory usage ranking:
;;   2:
;;     edges
;;   1:
;;     edge-by-X * 2
;;   0:
;;     concepts
;;     cat->concepts
;;     category*, predicate*

;; memory-usage: 0 1 2 3
(define (make-db db-dir)
  (define (db-path fname) (expand-user-path (build-path db-dir fname)))
  (define (open-db-path fname) (open-input-file (db-path fname)))
  (define (open-db-path/optional fname)
    (if (file-exists? (db-path fname))
        (open-db-path fname)
        (if (file-exists? "/dev/null")
            (open-input-file "/dev/null")
            (open-input-file "nul"))))
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
  (define in-pubmed-edges (open-db-path/optional fnin-pubmed-edges))
  (define in-offset-pubmed-edges
    (open-db-path/optional (fname-offset fnin-pubmed-edges)))

  (define category*
    (list->vector (read-all-from-file (db-path fnin-categories))))
  (define predicate*
    (list->vector (read-all-from-file (db-path fnin-predicates))))
  (define concept*
    (list->vector
      (stream->list
        (stream-map cdr (port->stream-offset&values in-concepts)))))
  (define catid=>cid* (make-vector (vector-length category*) #f))
  (for ((catid (in-range 0 (vector-length category*))))
       (define cid* (detail-ref in-concepts-by-category
                                in-offset-concepts-by-category catid))
       (vector-set! catid=>cid* catid cid*))

  (define (catid->cid* catid) (vector-ref catid=>cid* catid))
  (define (cid->concept cid)  (vector-ref concept* cid))
  (define (eid->edge eid)     (detail-ref in-edges in-offset-edges eid))
  (define (cid&concept-stream)    (vector->stream-offset&values concept*))
  (define (eid&edge/props-stream) (port->stream-offset&values in-edges))
  (define (subject->edge-stream cid pid? cat? dst?)
    (stream-edges-by-X in-edges-by-subject in-offset-edges-by-subject
                       cid pid? cat? dst?))
  (define (object->edge-stream cid pid? cat? dst?)
    (stream-edges-by-X in-edges-by-object in-offset-edges-by-object
                       cid pid? cat? dst?))
  (define (pmid->eid* pmid)
    (define pmid&eid*
      (detail-find in-pubmed-edges in-offset-pubmed-edges
                   (lambda (x)
                     (define key (car x))
                     (cond ((string<? key pmid) -1)
                           ((string=? key pmid)  0)
                           (else                 1)))))
    (if (eof-object? pmid&eid*) '() (cdr pmid&eid*)))
  (define (pmid&eid*-stream) (detail-stream in-pubmed-edges))

  (vector category* predicate* #t catid->cid* cid->concept eid->edge
          cid&concept-stream eid&edge/props-stream
          subject->edge-stream object->edge-stream
          pmid->eid* pmid&eid*-stream))

(define (db:category*             db)        (vector-ref db 0))
(define (db:predicate*            db)        (vector-ref db 1))
(define (db:catid->cid*           db . args) (apply (vector-ref db 3) args))
(define (db:cid->concept          db . args) (apply (vector-ref db 4) args))
(define (db:eid->edge             db . args) (apply (vector-ref db 5) args))
(define (db:cid&concept-stream    db)        ((vector-ref db 6)))
(define (db:eid&edge/props-stream db)        ((vector-ref db 7)))
(define (db:subject->edge-stream  db . args) (apply (vector-ref db 8) args))
(define (db:object->edge-stream   db . args) (apply (vector-ref db 9) args))
(define (db:pmid->eid*            db . args) (apply (vector-ref db 10) args))
(define (db:pmid&eid*-stream      db)        ((vector-ref db 11)))

(define (db:catid->category db catid) (vector-ref (db:category* db) catid))
(define (db:pid->predicate db pid)    (vector-ref (db:predicate* db) pid))

(define chars:ignore-typical "-")
(define chars:split-typical "\t\n\v\f\r !\"#$%&'()*+,./:;<=>?@\\[\\\\\\]\\^_`{|}~")

(define (~string*->offset&value*
          case-sensitive? chars:ignore chars:split offset&value* str* v->str)
  (define re:ignore (and (non-empty-string? chars:ignore)
                         (pregexp (string-append "[" chars:ignore "]"))))
  (define re:split (and (non-empty-string? chars:split)
                        (pregexp (string-append "[" chars:split "]"))))
  (define (normalize s case-sensitive?)
    (define pruned (if re:ignore (string-replace s re:ignore "") s))
    (if case-sensitive? pruned (string-downcase pruned)))
  (define (contains-upcase? s) (not (string=? s (string-downcase s))))
  (define case-sensitive?*
    (map (lambda (s) (or case-sensitive? (contains-upcase? s))) str*))
  (define needles
    (map (lambda (v case-sensitive?) (normalize v case-sensitive?))
         str* case-sensitive?*))
  (define (p? v)
    (define hay (v->str (cdr v)))
    (and hay
         (andmap
           (if re:split
             (lambda (n case-sensitive?)
               (ormap (lambda (s) (string=? s n))
                      (string-split (normalize hay case-sensitive?) re:split)))
             (lambda (n case-sensitive?)
               (string-contains? (normalize hay case-sensitive?) n)))
           needles case-sensitive?*)))
  (stream-filter p? offset&value*))

(define (simple~string->offset&value* offset&value* str v->str)
  (~string*->offset&value* #f "" "" offset&value* (list str) v->str))

(define (db:~category->catid&category* db ~category)
  (simple~string->offset&value*
    (vector->stream-offset&values (db:category* db))
    ~category (lambda (v) v)))
(define (db:~predicate->pid&predicate* db ~predicate)
  (simple~string->offset&value*
    (vector->stream-offset&values (db:predicate* db))
    ~predicate (lambda (v) v)))
(define (db:~name->cid&concept* db ~name)
  (simple~string->offset&value* (db:cid&concept-stream db) ~name concept-name))
(define (db:~cui->cid&concept* db ~cui)
  (simple~string->offset&value* (db:cid&concept-stream db) ~cui concept-cui))
(define (db:~name*->cid&concept*/options
          case-sensitive? chars:ignore chars:split db ~name*)
  (~string*->offset&value* case-sensitive? chars:ignore chars:split
                           (db:cid&concept-stream db) ~name* concept-name))
