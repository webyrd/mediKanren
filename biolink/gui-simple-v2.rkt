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


(displayln "Starting mediKanren Explorer...")

(displayln "Loading data sources...")


(displayln "loading semmed")
(define semmed (time (make-db "data/semmed")))
(displayln "loading monarch-lite")
(define monarch (time (make-db "data/monarch-lite")))
(displayln "loading rtx")
(define rtx (time (make-db "data/rtx")))
(displayln "loading scigraph")
(define scigraph (time (make-db "data/scigraph")))

(displayln "Finished loading data sources")


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


(define (edgeo e)
  (conde
    ((fresh (ee) (== `(semmed . ,ee) e) (db:edgeo semmed ee)))
    ((fresh (ee) (== `(monarch . ,ee) e) (db:edgeo monarch ee)))
    ((fresh (ee) (== `(rtx . ,ee) e) (db:edgeo rtx ee)))
    ((fresh (ee) (== `(scigraph . ,ee) e) (db:edgeo scigraph ee)))))

(define (fuzzy-concepto n c)
  (conde
    ((fresh (cc) (== `(semmed . ,cc) c) (db:~name-concepto semmed n cc)))
    ((fresh (cc) (== `(monarch . ,cc) c) (db:~name-concepto monarch n cc)))
    ((fresh (cc) (== `(rtx . ,cc) c) (db:~name-concepto rtx n cc)))
    ((fresh (cc) (== `(scigraph . ,cc) c) (db:~name-concepto scigraph n cc)))))



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
(define *concept-1-choices* (box '()))
(define *predicate-1-choices* (box '()))


(define (concept-list parent parent-search-panel parent-list-boxes-panel label name-string choices predicate-list-box-thunk predicate-choices edge-type)
  (define name-field (new text-field%
                          (label label)
                          (parent parent-search-panel)
                          (init-value "")
                          (callback (lambda (self event)
                                      (define name (send self get-value))
                                      (set-box! name-string name)
                                      (set-box! predicate-choices '())
                                      (send (predicate-list-box-thunk) set '())
                                      (handle)))))
  (define concept-listbox (new list-box%
                               (label label)
                               (choices '())
                               (columns '("DB" "CUI" "Name"))
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
                                                 (fresh (e s o st ot pubref)
                                                   (== e `(,s ,o ,predicate ,st ,ot  ,pubref))
                                                   (case edge-type
                                                     [(in-edge)
                                                      (membero o selected-concepts)]
                                                     [(out-edge)
                                                      (membero s selected-concepts)]
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
                                        ;(printf "predicates: ~s\n" predicates)
                                           (set-box! predicate-choices predicates)
                                           (send (predicate-list-box-thunk) set predicates)))))
  (define (mk-run)
    (let ((ans (if (equal? current-name "") ;; FIXME -- handle spaces, tabs, whatever (regex for all whitespace)
                   '()
                   (run* (q) (fuzzy-concepto current-name q)))))
      (let ((ans (remove-duplicates ans)))
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
                        (format "~a" cui)]))
                   ans)
              (map (lambda (x)
                     (match x
                       [`(,db-name ,cid ,cui ,name (,catid . ,cat) . ,props)
                        (~a name #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                   ans))
        ;; unselect all items
        (for ([i (length ans)])
             (send concept-listbox select i #f)))))
  (define current-name "")
  (define pending-name current-name)
  (define mk-thread #f)
  (define timer (new timer% (notify-callback
                             (lambda () (set! mk-thread (thread mk-run))))))
  (define (handle)
    (define new-name (send name-field get-value))
    (when (not (equal? current-name new-name))
      (set! current-name new-name)
      (and mk-thread (begin (kill-thread mk-thread) (set! mk-thread #f)))
      (send timer stop)
      (send timer start input-response-latency #t)))
  concept-listbox)

(define (launch-gui)
  (let ((frame (new frame%
                    (label "mediKanren Explorer v0.2")
                    (width HORIZ-SIZE)
                    (height VERT-SIZE))))
    (define concept-1-search-panel (new horizontal-panel%
                                        (parent frame)
                                        (alignment '(left center))
                                        (stretchable-height #f)))
    (define concept-1-list-boxes-panel (new horizontal-panel%
                                            (parent frame)
                                            (alignment '(left center))))
    (define concept-1-list-box (concept-list frame concept-1-search-panel concept-1-list-boxes-panel "Concept 1" *concept-1-name-string* *concept-1-choices* (lambda () predicate-1-list-box) *predicate-1-choices* 'out-edge))
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
    (send frame show #t)
    ))

(displayln
  "Launching GUI")

(launch-gui)
