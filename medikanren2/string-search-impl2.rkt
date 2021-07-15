#lang racket
(provide
  (except-out
    (all-defined-out)
    concept-name
  )
 )
(require racket/dict)
(require racket/vector)
(require "base.rkt")
(require "string-search-config.rkt")
(require "string-search-impl.rkt")


;;; schema-aware wrapping for string-search-impl.rkt functions

(define (foffs->concept fd-corpus foffs)
  (file-position fd-corpus foffs)
  (decode fd-corpus schema-pri))

(define (make-~name*->value* absdOut fn-cprop-primary fn-concept-name-index)
  (define fd-corpus
    (ensure-fd-input-binary (expand-user-path (build-path absdOut fn-cprop-primary))))
  (define fd-index
    (ensure-fd-input-binary (expand-user-path (build-path absdOut fn-concept-name-index))))
  (define (cid->concept foffs)
    (let ((v (foffs->concept fd-corpus foffs)))
;      (printf "found file offset offs=~a v=~a\n" offs v)
      (list '() (name-from-pri v))
      ))
  (lambda (~name*)
    (map
      (lambda (foffs)
        (let ((v (foffs->concept fd-corpus foffs)))
          v))
      (suffix:corpus-find*/disk cid->concept fd-index ~name*))))

;;; string search options
(struct stsopt (
  case-sensitive? ;; case sensitivity flag
  chars:ignore    ;; ignored characters ('chars:ignore-typical' is pre-defined)
  chars:split     ;; characters to split target name on for exact matching ('chars:split-typical' is pre-defined)
) #:constructor-name new-stsopt
  #:name stsopt-t
  #:transparent)

(define (make-stsopt
    #:case-sensitive? (case-sensitive? #f)
    #:chars:ignore (chars:ignore "")
    #:chars:split (chars:split "")
  )
  (new-stsopt case-sensitive? chars:ignore chars:split))

(define stsopt-default (make-stsopt))

;;; Shims
(define (conde/databases . args) (error "not implemented"))
(define (db:cid->concept . args) (error "not implemented"))
(define (isao . args) (error "not implemented"))
(define (subject-predicateo . args) (error "not implemented"))
(define (object-predicateo . args) (error "not implemented"))
(define (project . args) (error "not implemented"))
(define (stream-refo . args) (error "not implemented"))
(define (i&v->i&d . args) (error "not implemented"))
(define (db:cui*->cids . args) (error "not implemented"))

;;; db:~name*->cids calls ~name*->cid* via vector lookup
;;; ~name*->cid* in medikanren 1 is currently called make-~name*->cid* in medikanren 2

;;; from gui-simple-v2.rkt
;; :77
(define (split-name-string name)
  (string-split name #px"\\s+"))

;;; common.rkt:421
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


;;; From mk-db.rkt:51
(define (db:~cui*-concepto db ~cui* concept)
  (project (~cui*)
    (stream-refo
      (stream-map (i&v->i&d db) (db:~cui*->cid&concept* db ~cui*)) concept)))
;;; :63
;(define (db:~name*-concepto/options
(define (db:~name*->concept*/options1
          case-sensitive? chars:ignore chars:split rel ~name*)
  (define absdOut (hash-ref (relation-definition-info rel) 'path))
  (define fd-corpus
    (ensure-fd-input-binary (expand-user-path (build-path absdOut fn-cprop-primary))))
      (stream-map (lambda (foffs) (foffs->concept fd-corpus foffs))
                  ((lambda (arg . args) #f) ;db:~name*->cid&concept*/options
                    case-sensitive? chars:ignore chars:split rel ~name*))
)


;;; From db.rkt:223
(define (db:~name*->cids          db ~name*) ((vector-ref db 3) ~name*))
;;; :242
(define chars:ignore-typical "-")
(define chars:split-typical "\t\n\v\f\r !\"#$%&'()*+,./:;<=>?@\\[\\\\\\]\\^_`{|}~")

;;; :245
(define (smart-string-matches? stsopt str* hay)
  (match stsopt
      ( (struct stsopt-t (case-sensitive? chars:ignore chars:split))
        (smart-string-matches-impl? case-sensitive? chars:ignore chars:split str* hay))))
(define (smart-string-matches-impl? case-sensitive? chars:ignore chars:split str* hay)
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
          stsopt value* str*)
  (define (p? v)
    (define hay (name-from-pri v))
    (smart-string-matches? stsopt str* hay))
  (filter p? value*))
;;; :288
(define (db:~cui*->cid&concept* db ~cui*)
  (define cids (db:cui*->cids db ~cui*))
  (foldr (lambda (i cs) (stream-cons (cons i (db:cid->concept db i)) cs))
         '() cids))
;;; :295
;(define (db:~name*->cid&concept*/options
(define (db:~name*->concept*/options2
          stsopt absdOut fn-cprop-primary fn-concept-name-index ~name*)
  (define fd-corpus
    (ensure-fd-input-binary (expand-user-path (build-path absdOut fn-cprop-primary))))
  (define lookup (make-~name*->value* absdOut fn-cprop-primary fn-concept-name-index))
  (define value* (lookup ~name*))
  (~string*->offset&value* stsopt
                           value* ~name*))

;;; repr.rkt:62
(define (concept-name c)     (vector-ref c 2))
