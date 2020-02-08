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
  db:subject->pids
  db:object->pids
  db:subject->edge-stream
  db:object->edge-stream
  db:pmid->eid*
  db:pmid&eid*-stream
  db:xref->cid*

  db:~category->catid&category*
  db:~predicate->pid&predicate*
  db:~cui*->cid&concept*
  db:~cui->cid&concept*
  db:~name*->cid&concept*/options
  db:~name->cid&concept*

  chars:ignore-typical
  chars:split-typical

  smart-string-matches?
  )

(require
  "repr.rkt"
  "string-search.rkt"
  racket/stream
  racket/string
  racket/vector)

(define fnin-concepts             "concepts.scm")
(define fnin-xrefs                "xrefs.scm")
(define fnin-concepts-by-xref     "concepts-by-xref.scm")
(define fnin-concept-cui-corpus   "concept-cui-corpus.scm")
(define fnin-concept-cui-index    "concept-cui-index.bytes")
(define fnin-concept-name-corpus  "concept-name-corpus.scm")
(define fnin-concept-name-index   "concept-name-index.bytes")
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
  (define in-concepts-by-category (open-db-path fnin-concepts-by-category))
  (define in-offset-concepts-by-category
    (open-db-path (fname-offset fnin-concepts-by-category)))
  (define in-concepts-by-xref (open-db-path fnin-concepts-by-xref))
  (define in-offset-concepts-by-xref
    (open-db-path (fname-offset fnin-concepts-by-xref)))
  (define in-concepts            (open-db-path fnin-concepts))
  (define in-xrefs               (open-db-path fnin-xrefs))
  (define in-offset-xrefs        (open-db-path (fname-offset fnin-xrefs)))
  (define in-concept-cui-corpus  (open-db-path fnin-concept-cui-corpus))
  (define in-concept-cui-index   (open-db-path fnin-concept-cui-index))
  (define in-concept-name-corpus (open-db-path fnin-concept-name-corpus))
  (define in-concept-name-index  (open-db-path fnin-concept-name-index))
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

  (define cui-corpus
    (for/vector ((x (port->stream-offset&values in-concept-cui-corpus)))
                (cdr x)))
  (close-input-port in-concept-cui-corpus)
  (define cui-index (port->string-keys in-concept-cui-index))
  (close-input-port in-concept-cui-index)
  (define name-corpus
    (for/vector ((x (port->stream-offset&values in-concept-name-corpus)))
                (cdr x)))
  (close-input-port in-concept-name-corpus)
  (define name-index (port->suffix-keys in-concept-name-index))
  (close-input-port in-concept-name-index)

  (define catid=>cid* (make-vector (vector-length category*) #f))
  (for ((catid (in-range 0 (vector-length category*))))
       (define cid* (detail-ref in-concepts-by-category
                                in-offset-concepts-by-category catid))
       (vector-set! catid=>cid* catid cid*))
  (close-input-port in-concepts-by-category)
  (close-input-port in-offset-concepts-by-category)

  (define (xref->cid* xref)
    (define xid (detail-find-index in-xrefs in-offset-xrefs
                                   (lambda (key)
                                     (cond ((string<? key xref) -1)
                                           ((string=? key xref)  0)
                                           (else                 1)))))
    (if xid (detail-ref in-concepts-by-xref in-offset-concepts-by-xref xid)
      '()))

  (define (catid->cid* catid)  (vector-ref catid=>cid* catid))
  (define (cid->concept cid)   (detail-ref in-concepts in-offset-concepts cid))
  (define (cid&concept-stream) (port->stream-offset&values in-concepts))
  (define (eid->edge eid)         (detail-ref in-edges in-offset-edges eid))
  (define (eid&edge/props-stream) (port->stream-offset&values in-edges))

  (define (subject->pids cid)
    (edge-pids-by-X in-edges-by-subject in-offset-edges-by-subject cid))
  (define (object->pids cid)
    (edge-pids-by-X in-edges-by-object in-offset-edges-by-object cid))
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

  (vector category* predicate*
          (cons cui-corpus cui-index)
          (cons name-corpus name-index)
          catid->cid* cid->concept eid->edge
          cid&concept-stream eid&edge/props-stream
          subject->edge-stream object->edge-stream
          pmid->eid* pmid&eid*-stream subject->pids object->pids
          xref->cid*))

(define (db:category*             db)        (vector-ref db 0))
(define (db:predicate*            db)        (vector-ref db 1))
(define (db:concept-cui-corpus    db)        (car (vector-ref db 2)))
(define (db:concept-cui-index     db)        (cdr (vector-ref db 2)))
(define (db:concept-name-corpus   db)        (car (vector-ref db 3)))
(define (db:concept-name-index    db)        (cdr (vector-ref db 3)))
(define (db:catid->cid*           db . args) (apply (vector-ref db 4) args))
(define (db:cid->concept          db . args) (apply (vector-ref db 5) args))
(define (db:eid->edge             db . args) (apply (vector-ref db 6) args))
(define (db:cid&concept-stream    db)        ((vector-ref db 7)))
(define (db:eid&edge/props-stream db)        ((vector-ref db 8)))
(define (db:subject->edge-stream  db . args) (apply (vector-ref db 9) args))
(define (db:object->edge-stream   db . args) (apply (vector-ref db 10) args))
(define (db:pmid->eid*            db . args) (apply (vector-ref db 11) args))
(define (db:pmid&eid*-stream      db)        ((vector-ref db 12)))
(define (db:subject->pids         db . args) (apply (vector-ref db 13) args))
(define (db:object->pids          db . args) (apply (vector-ref db 14) args))
(define (db:xref->cid*            db . args) (apply (vector-ref db 15) args))

(define (db:catid->category db catid) (vector-ref (db:category* db) catid))
(define (db:pid->predicate db pid)    (vector-ref (db:predicate* db) pid))

(define chars:ignore-typical "-")
(define chars:split-typical "\t\n\v\f\r !\"#$%&'()*+,./:;<=>?@\\[\\\\\\]\\^_`{|}~")

(define (smart-string-matches? case-sensitive? chars:ignore chars:split str* hay)
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
  (and hay
       (andmap
        (if re:split
            (lambda (n case-sensitive?)
              (ormap (lambda (s) (string=? s n))
                     (string-split (normalize hay case-sensitive?) re:split)))
            (lambda (n case-sensitive?)
              (string-contains? (normalize hay case-sensitive?) n)))
        needles case-sensitive?*)))

(define (~string*->offset&value*
          case-sensitive? chars:ignore chars:split offset&value* str* v->str)
  (define (p? v)
    (define hay (v->str (cdr v)))
    (smart-string-matches? case-sensitive? chars:ignore chars:split str* hay))
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

(define (db:~cui*->cid&concept* db ~cui*)
  (define cids (string:corpus-find* (db:concept-cui-corpus db)
                                    (db:concept-cui-index db) ~cui*))
  (foldr (lambda (i cs) (stream-cons (cons i (db:cid->concept db i)) cs))
         '() cids))
(define (db:~cui->cid&concept* db ~cui)
  (db:~cui*->cid&concept* db (list ~cui)))

(define (db:~name*->cid&concept*/options
          case-sensitive? chars:ignore chars:split db ~name*)
  (define cids (suffix:corpus-find* (db:concept-name-corpus db)
                                    (db:concept-name-index db) ~name*))
  (define found (foldr (lambda (i cs)
                         (stream-cons (cons i (db:cid->concept db i)) cs))
                       '() cids))
  (~string*->offset&value* case-sensitive? chars:ignore chars:split
                           found ~name* concept-name))
(define (db:~name->cid&concept* db ~name)
  (simple~string->offset&value* (db:cid&concept-stream db) ~name concept-name))
