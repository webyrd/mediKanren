#lang racket
;; This file is private to string-search.rkt.
;; Applications should include string-search.rkt instead.
(provide
    (all-defined-out)
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

;;; from gui-simple-v2.rkt
;; :77
(define (split-name-string name)
  (string-split name #px"\\s+"))

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
;;; :295
(define (db:~name*->concept*/options
          stsopt absdOut fn-cprop-primary fn-concept-name-index ~name*)
  (define fd-corpus
    (ensure-fd-input-binary (expand-user-path (build-path absdOut fn-cprop-primary))))
  (define lookup (make-~name*->value* absdOut fn-cprop-primary fn-concept-name-index))
  (define value* (lookup ~name*))
  (~string*->offset&value* stsopt
                           value* ~name*))
