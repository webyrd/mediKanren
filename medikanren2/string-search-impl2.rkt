#lang racket
(provide
    find-concepts/options
 )
(require racket/dict)
(require racket/vector)
(require "dbk/mk.rkt")

;;; Shims
(define (conde/databases) (error "not implemented"))
(define (db:cid->concept) (error "not implemented"))
(define (isao) (error "not implemented"))
(define (subject-predicateo) (error "not implemented"))
(define (object-predicateo) (error "not implemented"))
(define (project) (error "not implemented"))
(define (stream-refo) (error "not implemented"))
(define (i&v->i&d) (error "not implemented"))
(define (db:cui*->cids) (error "not implemented"))

;;; db:~name*->cids calls ~name*->cid* via vector lookup
;;; ~name*->cid* in medikanren 1 is currently called make-~name*->cid* in medikanren 2

;;; From common.rkt:182
(define (~name*-concepto ~name* concept)
  (conde/databases
    (lambda (dbname db)
      (fresh (c)
        (== `(,dbname . ,c) concept)
        (db:~name*-concepto/options
          #f ;; case sensitivity flag
          "" ;; ignored characters ('chars:ignore-typical' is pre-defined)
          "" ;; characters to split target name on for exact matching ('chars:split-typical' is pre-defined)
          db ~name* c)))))
(define (~cui*-concepto ~cui* concept)
  (conde/databases
    (lambda (dbname db)
      (fresh (c) (== `(,dbname . ,c) concept)
        (db:~cui*-concepto db ~cui* c)))))
;;; :421
(define (find-isa-concepts count concepts)
  (remove-duplicates (run count (s/db)
                       (fresh (o/db)
                         (membero o/db concepts)
                         (isao s/db o/db)))))
;;; :427
(define (concepts/options subject? object? isa-count concepts)
  ;; subject? and object? insist that a concept participate in a certain role.
  ;; If via-cui? then strings is an OR-list of CUIs to consider.
  ;; Otherwise, strings is an AND-list of fragments the name must contain.
  (let* ((isa-concepts (find-isa-concepts isa-count concepts))
         (ans (if (null? isa-concepts) (remove-duplicates concepts)
                (remove-duplicates (append concepts isa-concepts))))
         (ans (filter  ;; Only include concepts with at least one predicate.
                (lambda (concept)
                  (define (? cpo) (not (null? (run 1 (p) (cpo concept p)))))
                  (and (or (not subject?) (? subject-predicateo))
                       (or (not object?)  (? object-predicateo))))
                ans)))
    (sort ans (lambda (a1 a2)
                (let ((dbname1 (symbol->string (car a1)))
                      (cui1 (caddr a1))
                      (dbname2 (symbol->string (car a2)))
                      (cui2 (caddr a2)))
                  (or (string>? dbname1 dbname2)
                      (and (string=? dbname1 dbname2)
                           (string<? cui1 cui2))))))))
;;; :457
(define (find-concepts/options subject? object? isa-count via-cui? strings)
  (concepts/options subject? object? isa-count
                    (if via-cui?
                      (run* (c) (~cui*-concepto strings c))
                      (run* (c) (~name*-concepto strings c)))))


;;; From mk-db.rkt:51
(define (db:~cui*-concepto db ~cui* concept)
  (project (~cui*)
    (stream-refo
      (stream-map (i&v->i&d db) (db:~cui*->cid&concept* db ~cui*)) concept)))
;;; :63
(define (db:~name*-concepto/options
          case-sensitive? chars:ignore chars:split db ~name* concept)
  (project (~name*)
    (stream-refo
      (stream-map (i&v->i&d db)
                  (db:~name*->cid&concept*/options
                    case-sensitive? chars:ignore chars:split db ~name*))
      concept)))


;;; From db.rkt:223
(define (db:~name*->cids          db ~name*) ((vector-ref db 3) ~name*))
;;; :245
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
;;; :269
(define (~string*->offset&value*
          case-sensitive? chars:ignore chars:split offset&value* str* v->str)
  (define (p? v)
    (define hay (v->str (cdr v)))
    (smart-string-matches? case-sensitive? chars:ignore chars:split str* hay))
  (stream-filter p? offset&value*))
;;; :288
(define (db:~cui*->cid&concept* db ~cui*)
  (define cids (db:cui*->cids db ~cui*))
  (foldr (lambda (i cs) (stream-cons (cons i (db:cid->concept db i)) cs))
         '() cids))
;;; :295
(define (db:~name*->cid&concept*/options
          case-sensitive? chars:ignore chars:split db ~name*)
  (define cids (db:~name*->cids db ~name*))
  (define found (foldr (lambda (i cs)
                         (stream-cons (cons i (db:cid->concept db i)) cs))
                       '() cids))
  (~string*->offset&value* case-sensitive? chars:ignore chars:split
                           found ~name* concept-name))

;;; repr.rkt:62
(define (concept-name c)     (vector-ref c 2))
