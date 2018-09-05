#lang racket

(require
  racket/sandbox
  racket/gui/base
  racket/engine
  racket/date
  "db.rkt"
  "mk-db.rkt"
  (except-in racket/match ==))

(provide
  launch-gui)

#|
concept format (subject or object), without dbname at front:

`(,cid ,cui ,name (,catid . ,cat) . ,props)

concept format (subject or object), with dbname at front (as used in fuzzy-concepto):

`(,dbname ,cid ,cui ,name (,catid . ,cat) . ,props)


edge format, without dbname at front:

`(,eid (,scid ,scui ,sname (,scatid . ,scat) . ,sprops)
       (,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops)
       (,pid . ,pred) . ,eprops)

edge format, with dbname at front (as used in edgeo):

`(,dbname ,eid (,scid ,scui ,sname (,scatid . ,scat) . ,sprops)
               (,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops)
               (,pid . ,pred) . ,eprops)
|#



(displayln "Starting mediKanren Explorer...")

(displayln "Loading data sources...")


(displayln "loading semmed")
(define semmed (time (make-db "data/semmed")))
(displayln "loading monarch-lite")
(define monarch (time (make-db "data/monarch-lite")))
(displayln "loading rtx")
(define rtx (time (make-db "data/rtx")))
;(displayln "loading scigraph")
;(define scigraph (time (make-db "data/scigraph")))

(displayln "Finished loading data sources")



;;; TODO
;;;
;;; recognize concept name containing only whitespace as being empty
;;;
;;; use a reasonable default sorting for the concepts
;;;
;;; add ability to sort concept columns by clicking on column titles
;;;
;;; speed up concept search--seems much slower than in the old mediKanren
;;; (perhaps consider searching one DB exhaustively before moving to the next one)
;;;
;;; add ability to include only some of the data sources
;;;
;;; add ability to show and hide concept columns
;;;
;;; add ability to show date of data sources, and date/provenance for results
;;;
;;; add ability to specify/filter by data source or category


;;; ISSUES
;;;
;;; Seem to be missing '<gene> has_phenotype alacrima' entries for
;;; HEXA and GBA, even though these seem to be in the Monarch website.
;;; Why?
;;;
;;; Different data sources have what are effectively duplicate entries,
;;; but it isn't always easy to tell.
;;;
;;; Different data sources have different category numbers for the
;;; same CUI/entity.  For example, 'Imatinib mesylate' has the 'UMLS:C0939537'
;;; CUI in both Semmed and Scigraph, yet the category is (5 . "chemical_substance")
;;; for Semmed and (21 . "chemical_substance") for Scigraph.  Why?
;;; (The concept IDs are different in this case)


;; list membership
(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        [(== x y)]
        [(=/= x y) (membero x rest)]))))

;; remove duplicates from a list
(define rem-dups
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(member (car ls) (cdr ls)) (rem-dups (cdr ls))]
      [else (cons (car ls) (rem-dups (cdr ls)))])))

#|
`(,dbname ,eid (,scid ,scui ,sname (,scatid . ,scat) . ,sprops)
               (,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops)
               (,pid . ,pred) . ,eprops)
|#
(define (edgeo e)
  (conde
    ((fresh (ee) (== `(semmed . ,ee) e) (db:edgeo semmed ee)))
    ((fresh (ee) (== `(monarch . ,ee) e) (db:edgeo monarch ee)))
    ((fresh (ee) (== `(rtx . ,ee) e) (db:edgeo rtx ee)))
    ;;((fresh (ee) (== `(scigraph . ,ee) e) (db:edgeo scigraph ee)))
    ))

#|
`(,db-name ,cid ,cui ,name (,catid . ,cat) . ,props)
|#
(define (fuzzy-concepto n c)
  (conde
    ((fresh (cc) (== `(semmed . ,cc) c) (db:~name-concepto semmed n cc)))
    ((fresh (cc) (== `(monarch . ,cc) c) (db:~name-concepto monarch n cc)))
    ((fresh (cc) (== `(rtx . ,cc) c) (db:~name-concepto rtx n cc)))
    ;;((fresh (cc) (== `(scigraph . ,cc) c) (db:~name-concepto scigraph n cc)))
    ))



(define (DECREASES pred)
  (fresh (_)
    (conde
      [(== `(,_ . "treats") pred)]
      [(== `(,_ . "prevents") pred)]
      [(== `(,_ . "negatively_regulates") pred)])))

(define (INCREASES pred)
  (fresh (_)
    (conde
      [(== `(,_ . "produces") pred)]
      [(== `(,_ . "positively_regulates") pred)])))



(define *verbose* #t)

;;; window size
(define HORIZ-SIZE 800)
(define VERT-SIZE 400)

(define input-response-latency 50)

(define MAX-CHAR-WIDTH 100)

(define DECREASES_PREDICATE_STRING "decreases [synthetic]")
(define INCREASES_PREDICATE_STRING "increases [synthetic]")
(define DECREASES_STAR_PREDICATE_STRING "decreases* [synthetic]")
(define INCREASES_STAR_PREDICATE_STRING "increases* [synthetic]")



(define *concept-1-name-string* (box ""))
(define *concept-1-isa-flag* (box #f))
(define *concept-1-choices* (box '()))
(define *predicate-1-choices* (box '()))

(define *concept-2-name-string* (box ""))
(define *concept-2-isa-flag* (box #f))
(define *concept-2-choices* (box '()))
(define *predicate-2-choices* (box '()))


(define (concept-list parent parent-search/isa-panel parent-list-boxes-panel label name-string isa-flag choices predicate-list-box-thunk predicate-choices edge-type)
  (define name-field (new text-field%
                          (label label)
                          (parent parent-search/isa-panel)
                          (init-value "")
                          (callback (lambda (self event)
                                      (define name (send self get-value))
                                      (set-box! name-string name)
                                      (set-box! predicate-choices '())
                                      (send (predicate-list-box-thunk) set '())
                                      (handle)))))
  (define isa-field (new check-box%
                         (parent parent-search/isa-panel)
                         (label "Include ISA-related concepts")
                         (value #f)
                         (callback (lambda (self event) (handle)))))
  (define concept-listbox (new list-box%
                               (label label)
                               (choices '())
                               (columns '("DB" "CID" "CUI" "Category" "Name"))
                               (parent parent-list-boxes-panel)
                               (style '(column-headers extended))
                               (callback (lambda (self event)
                                           (define selections (send self get-selections))
                                           (define selected-concepts (foldr (lambda (i l) (cons (list-ref (unbox choices) i) l)) '() selections))
                                           (when *verbose*
                                             (printf "selected concepts:\n~s\n" selected-concepts))
                                           (define predicates
                                             (sort
                                              (remove-duplicates
                                               (run* (predicate)
                                                 (fresh (dbname e eid s o pid eprops)
                                                   (== `(,dbname ,eid ,s ,o (,pid . ,predicate) . ,eprops) e)
                                                   (case edge-type
                                                     [(in-edge)
                                                      (membero `(,dbname . ,o) selected-concepts)]
                                                     [(out-edge)
                                                      (membero `(,dbname . ,s) selected-concepts)]
                                                     [else (error 'concept-listbox/predicates)])
                                                   (edgeo e))))
                                              string<?))
                                           (set! predicates (append
                                                             (list
                                                              DECREASES_PREDICATE_STRING
                                                              INCREASES_PREDICATE_STRING
                                                              DECREASES_STAR_PREDICATE_STRING
                                                              INCREASES_STAR_PREDICATE_STRING)
                                                             predicates))
                                           (printf "predicates: ~s\n" predicates)
                                           (set-box! predicate-choices predicates)
                                           (send (predicate-list-box-thunk) set predicates)))))
  (define (mk-run)
    (let ((ans (if (equal? current-name "") ;; FIXME -- handle spaces, tabs, whatever (regex for all whitespace)
                   '()
                   (run* (q) (fuzzy-concepto current-name q)))))
      (let ((ans (remove-duplicates ans)))
        (let ((isa-ans (if (and (not (equal? current-name "")) current-isa)  ;; FIXME -- handle spaces, tabs, whatever (regex for all whitespace)
                           ;; only grab the first 50
                           (remove-duplicates
                            (run 50 (s-with-dbname) ;; 50 should probably be a parameter
                              (fresh (o-with-dbname dbname o s eid pid eprops e)
                                (membero o-with-dbname ans)
                                (== `(,dbname . ,o) o-with-dbname)
                                (== `(,dbname ,eid ,s ,o (,pid . "subclass_of") . ,eprops) e)
                                (== `(,dbname . ,s) s-with-dbname)
                                (edgeo e))))
                        '())))
          (let ((ans (remove-duplicates (append ans isa-ans))))
            (set-box! choices ans)
            (send concept-listbox
                  set
                  (map (lambda (x)
                         (match x
                           [`(,db-name ,cid ,cui ,name (,catid . ,cat) . ,props)
                            (~a db-name #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                       ans)
                  (map (lambda (x)
                         (match x
                           [`(,db-name ,cid ,cui ,name (,catid . ,cat) . ,props)
                            (format "~a" cid)]))
                       ans)              
                  (map (lambda (x)
                         (match x
                           [`(,db-name ,cid ,cui ,name (,catid . ,cat) . ,props)
                            (format "~a" cui)]))
                       ans)
                  (map (lambda (x)
                         (match x
                           [`(,db-name ,cid ,cui ,name (,catid . ,cat) . ,props)
                            (~a `(,catid . ,cat) #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                       ans)              
                  (map (lambda (x)
                         (match x
                           [`(,db-name ,cid ,cui ,name (,catid . ,cat) . ,props)
                            (~a name #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                       ans))
            ;; unselect all items
            (for ([i (length ans)])
                 (send concept-listbox select i #f)))))))
  (define current-name "")
  (define current-isa #f)
  (define pending-name current-name)
  (define mk-thread #f)
  (define timer (new timer% (notify-callback
                             (lambda () (set! mk-thread (thread mk-run))))))
  (define (handle)
    (define new-name (send name-field get-value))
    (define new-isa (send isa-field get-value))
    (when (not (and (equal? current-name new-name)
                    (equal? current-isa new-isa)))
      (set! current-name new-name)
      (set! current-isa new-isa)
      (set-box! isa-flag current-isa)
      (and mk-thread (begin (kill-thread mk-thread) (set! mk-thread #f)))
      (send timer stop)
      (send timer start input-response-latency #t)))
  concept-listbox)

(define (launch-gui)
  (let ((frame (new frame%
                    (label "mediKanren Explorer v0.2")
                    (width HORIZ-SIZE)
                    (height VERT-SIZE))))
    (define concept-1-search/isa-panel (new horizontal-panel%
                                            (parent frame)
                                            (alignment '(left center))
                                            (stretchable-height #f)))
    (define concept-1-list-boxes-panel (new horizontal-panel%
                                            (parent frame)
                                            (alignment '(left center))))
    (define concept-1-list-box (concept-list frame concept-1-search/isa-panel concept-1-list-boxes-panel "Concept 1" *concept-1-name-string* *concept-1-isa-flag* *concept-1-choices* (lambda () predicate-1-list-box) *predicate-1-choices* 'out-edge))
    (define predicate-1-list-box (new list-box%
                                      (label "Predicate 1")
                                      (choices (unbox *predicate-1-choices*))
                                      (columns '("Name"))
                                      (parent concept-1-list-boxes-panel)
                                      (style '(extended))
                                      (callback (lambda (button event)
                                                  (void)))))
    (define edge-description (new message%
                                  (parent frame)
                                  (label "Concept 1 -> Predicate 1 -> [X] -> Predicate 2 -> Concept 2")))
    (define concept-2-search/isa-panel (new horizontal-panel%
                                            (parent frame)
                                            (alignment '(left center))
                                            (stretchable-height #f)))
    (define concept-2-list-boxes-panel (new horizontal-panel%
                                            (parent frame)
                                            (alignment '(left center))))
    (define predicate-2-list-box (new list-box%
                                      (label "Predicate 2")
                                      (choices (unbox *predicate-2-choices*))
                                      (columns '("Name"))
                                      (parent concept-2-list-boxes-panel)
                                      (style '(extended))
                                      (callback (lambda (button event)
                                                  (void)))))
    (define concept-2-list-box (concept-list frame concept-2-search/isa-panel concept-2-list-boxes-panel "Concept 2" *concept-2-name-string* *concept-2-isa-flag* *concept-2-choices* (lambda () predicate-2-list-box) *predicate-2-choices* 'out-edge))
    
    (send frame show #t)
    ))

(displayln
  "Launching GUI")

(launch-gui)
