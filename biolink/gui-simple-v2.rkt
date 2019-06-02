#lang racket

(require
  "common.rkt"
  racket/sandbox
  racket/gui/base
  framework
  racket/engine
  racket/date
  racket/string
  net/sendurl
  "db.rkt"
  "mk-db.rkt"
  (except-in racket/match ==)
  (only-in srfi/1 iota))

(provide
  launch-gui)

(define MEDIKANREN_VERSION_STRING "mediKanren Explorer 0.2.24")

(define argv (current-command-line-arguments))
(define argv-optional '#(CONFIG_FILE))

(when (not (<= (vector-length argv) (vector-length argv-optional)))
  (error "optional arguments ~s; given ~s" argv-optional argv))

(displayln "Starting mediKanren Explorer...")
(displayln MEDIKANREN_VERSION_STRING)
;; Loading will occur at first use if not explicitly forced like this.
(load-config #t (and (<= 1 (vector-length argv)) (vector-ref argv 0)))
(load-databases #t)

;;; Query save file settings
(define WRITE_QUERY_RESULTS_TO_FILE            (config-ref 'query-results.write-to-file?))
(define QUERY_RESULTS_FILE_NAME                (config-ref 'query-results.file-name))
(define HUMAN_FRIENDLY_QUERY_RESULTS_FILE_NAME (config-ref 'query-results.file-name-human))
(define SPREADSHEET_FRIENDLY_QUERY_RESULTS_FILE_NAME (config-ref 'query-results.file-name-spreadsheet))
(define QUERY_RESULTS_FILE_MODE                (config-ref 'query-results.file-mode))

;;; Initial window size
(define HORIZ-SIZE (config-ref 'initial-window-size.horizontal))
(define VERT-SIZE (config-ref 'initial-window-size.vertical))

;;; Decreases/increases predicate names
(define DECREASES_PREDICATE_NAMES (config-ref 'decreases-predicate-names))
(define INCREASES_PREDICATE_NAMES (config-ref 'increases-predicate-names))


#|
concept format (subject or object), without dbname at front:

`(,cid ,cui ,name (,catid . ,cat) ,props)

concept format (subject or object), with dbname at front:

`(,dbname ,cid ,cui ,name (,catid . ,cat) ,props)


edge format, without dbname at front:

`(,eid (,scid ,scui ,sname (,scatid . ,scat) ,sprops)
       (,ocid ,ocui ,oname (,ocatid . ,ocat) ,oprops)
       (,pid . ,pred) ,eprops)

edge format, with dbname at front (as used in edgeo):

`(,dbname ,eid (,scid ,scui ,sname (,scatid . ,scat) ,sprops)
               (,ocid ,ocui ,oname (,ocatid . ,ocat) ,oprops)
               (,pid . ,pred) ,eprops)
|#

(define (split-name-string name)
  (string-split name #px"\\s+"))

(define *verbose* #t)

(define input-response-latency 50)

(define MAX-CHAR-WIDTH 150)

(define smart-column-width-list-box%
  (class list-box%
    (super-new)
    (define (on-size width height)
      (super on-size width height)
      (set-default-column-widths this))
    (override on-size)))

(define (set-default-column-widths list-box)
  (define label* (send list-box get-column-labels))
  (define num-cols (length label*))
  (define window-width (send list-box get-width))
  (define min-width 5)
  (define max-width 1000)
  (define fudge-factor 4) ;; column divider width
  (define width (min (max (- (floor (/ window-width num-cols)) fudge-factor)
                          min-width)
                     max-width))
  (let loop ((col-num (sub1 num-cols)))
    (cond
      [(zero? col-num) (void)]
      [else
       (send list-box
             set-column-width
             col-num
             width
             min-width	 
             max-width)
       (loop (sub1 col-num))])))

(define construct-predicate-label-string
  (lambda (pred-string pred-name-list)
    (~a
     (string-append pred-string
                   "  ("
                   (foldr (lambda (str1 str2)
                            (if (equal? "" str2)
                                (string-append str1 "" str2)
                                (string-append str1 ", " str2)))
                          ""
                          pred-name-list)
                   ")")
     #:max-width MAX-CHAR-WIDTH #:limit-marker "...")))

(define DECREASES_PREDICATE_PREFIX_STRING "decreases [synthetic]")
(define DECREASES_PREDICATE_STRING
  (construct-predicate-label-string DECREASES_PREDICATE_PREFIX_STRING DECREASES_PREDICATE_NAMES))
(define INCREASES_PREDICATE_PREFIX_STRING "increases [synthetic]")
(define INCREASES_PREDICATE_STRING
  (construct-predicate-label-string INCREASES_PREDICATE_PREFIX_STRING INCREASES_PREDICATE_NAMES))

(define SYNTHETIC_PREDICATE_PREFIXES (list DECREASES_PREDICATE_PREFIX_STRING
                                           INCREASES_PREDICATE_PREFIX_STRING
                                           ))


(define SORT_COLUMN_INCREASING 'sort-column-increasing)
(define SORT_COLUMN_DECREASING 'sort-column-decreasing)

(define *concept-1-column-sort-order*
  (vector SORT_COLUMN_INCREASING
          SORT_COLUMN_INCREASING
          SORT_COLUMN_INCREASING
          SORT_COLUMN_INCREASING
          SORT_COLUMN_INCREASING))
(define *last-concept-1-column-clicked-for-sorting* (box -1))

(define *concept-2-column-sort-order*
  (vector SORT_COLUMN_INCREASING
          SORT_COLUMN_INCREASING
          SORT_COLUMN_INCREASING
          SORT_COLUMN_INCREASING
          SORT_COLUMN_INCREASING))
(define *last-concept-2-column-clicked-for-sorting* (box -1))

(define *concept-X-column-sort-order*
  (vector SORT_COLUMN_INCREASING
          SORT_COLUMN_INCREASING
          SORT_COLUMN_INCREASING
          SORT_COLUMN_INCREASING
          SORT_COLUMN_INCREASING
          SORT_COLUMN_DECREASING
          SORT_COLUMN_DECREASING
          SORT_COLUMN_DECREASING
          SORT_COLUMN_DECREASING))
(define *last-concept-X-column-clicked-for-sorting* (box -1))


(define *concept-1-name-string* (box ""))
(define *concept-1-isa-flag* (box #f))
(define *concept-1-choices* (box '()))
(define *predicate-1-choices* (box '()))

(define *concept-2-name-string* (box ""))
(define *concept-2-isa-flag* (box #f))
(define *concept-2-choices* (box '()))
(define *predicate-2-choices* (box '()))

(define *concept-X-choices* (box '()))
(define *full-path-choices* (box '()))
(define *pubmed-choices* (box '()))

;; saved choices used to generate
;; paths when clicking on a concept in the X list box.
(define *solution-concept-1-name-string* (box ""))
(define *solution-concept-2-name-string* (box ""))
(define *solution-concept-1-isa-flag* (box #f))
(define *solution-concept-2-isa-flag* (box #f))
(define *solution-concept-1-choices* (box '()))
(define *solution-concept-2-choices* (box '()))
(define *solution-predicate-1-choices* (box '()))
(define *solution-predicate-2-choices* (box '()))

(define (convert-concept-1/2-to-list-box-format concept)
  (match concept
    [`(,dbname ,cid ,cui ,name (,catid . ,cat) ,props)
     (list (format "~a" dbname)
           (format "~a" cid)
           (format "~a" cui)
           (format "~a" `(,catid . ,cat))
           (~a name #:max-width MAX-CHAR-WIDTH #:limit-marker "..."))]))

(define (convert-X-concept-to-list-box-format concept)
  (match concept
    [`(,dbname ,cid ,cui ,name (,catid . ,cat) ,props ,max-pubmed-count ,min-pubmed-count ,path-length ,confidence)
     (list (format "~a" dbname)
           (format "~a" cid)
           (format "~a" cui)
           (format "~a" `(,catid . ,cat))
           (~a name #:max-width MAX-CHAR-WIDTH #:limit-marker "...")
           (format "~a" max-pubmed-count)
           (format "~a" min-pubmed-count)
           (format "~a" path-length)
           (format "~a" confidence))]))

(define (convert-concept-1/2-to-column-sorting-format concept)
  (match concept
    [`(,dbname ,cid ,cui ,name (,catid . ,cat) ,props)     
     (list (format "~a" dbname)
           cid
           (format "~a" cui)
           catid
           (~a name #:max-width MAX-CHAR-WIDTH #:limit-marker "..."))]))

(define (convert-X-concept-to-column-sorting-format concept)
  (match concept
    [`(,dbname ,cid ,cui ,name (,catid . ,cat) ,props ,max-pubmed-count ,min-pubmed-count ,path-length ,confidence)
     (list (format "~a" dbname)
           cid
           (format "~a" cui)
           catid
           (~a name #:max-width MAX-CHAR-WIDTH #:limit-marker "...")
           max-pubmed-count
           min-pubmed-count
           path-length
           confidence)]))

(define (make-send-concepts-to-concept-1/2-list-box concept-1/2-list-box-thunk)
  (lambda (concepts)
    (define concept-1/2-list-box (concept-1/2-list-box-thunk))
    (define formatted-concepts (map convert-concept-1/2-to-list-box-format concepts))
    (send concept-1/2-list-box
          set
          (map (lambda (e) (list-ref e 0)) formatted-concepts)
          (map (lambda (e) (list-ref e 1)) formatted-concepts)
          (map (lambda (e) (list-ref e 2)) formatted-concepts)
          (map (lambda (e) (list-ref e 3)) formatted-concepts)
          (map (lambda (e) (list-ref e 4)) formatted-concepts))))

(define (make-send-concepts-to-concept-X-list-box concept-X-list-box)
  (lambda (concepts)
    (define formatted-concepts (map convert-X-concept-to-list-box-format concepts))
    (send concept-X-list-box
          set
          (map (lambda (e) (list-ref e 0)) formatted-concepts)
          (map (lambda (e) (list-ref e 1)) formatted-concepts)
          (map (lambda (e) (list-ref e 2)) formatted-concepts)
          (map (lambda (e) (list-ref e 3)) formatted-concepts)
          (map (lambda (e) (list-ref e 4)) formatted-concepts)
          (map (lambda (e) (list-ref e 5)) formatted-concepts)
          (map (lambda (e) (list-ref e 6)) formatted-concepts)
          (map (lambda (e) (list-ref e 7)) formatted-concepts)
          (map (lambda (e) (list-ref e 8)) formatted-concepts))))

(define (handle-sort-by-column-header-click event
                                            last-column-clicked-for-sorting-box
                                            column-sort-order-vector
                                            choices-box
                                            convert-values-to-column-sorting-format
                                            send-values-to-list-box)
  ;; sort by column
  (define column-clicked (send event get-column))
  (define last-column-clicked (unbox last-column-clicked-for-sorting-box))

  (define sort-order (vector-ref column-sort-order-vector column-clicked))

  ;; swap sort order if user clicks on same column twice in a row
  (when (= column-clicked last-column-clicked)
    (set! sort-order
          (if (eqv? sort-order SORT_COLUMN_INCREASING)
              SORT_COLUMN_DECREASING
              SORT_COLUMN_INCREASING))
    (vector-set! column-sort-order-vector
                 column-clicked
                 sort-order))

  (printf "sorting by column ~s in ~s order\n" column-clicked sort-order)

  (define choices (unbox choices-box))

  (define sorted-choices (sort choices
                               (lambda (c1 c2) 
                                 (let ((fc1 (convert-values-to-column-sorting-format c1))
                                       (fc2 (convert-values-to-column-sorting-format c2)))
                                   (let ((v1 (list-ref fc1 column-clicked))
                                         (v2 (list-ref fc2 column-clicked)))
                                     (let ((num-compare
                                            (if (eqv? sort-order SORT_COLUMN_INCREASING)
                                                <
                                                >))
                                           (string-compare
                                            (if (eqv? sort-order SORT_COLUMN_INCREASING)
                                                string<?
                                                string>?)))
                                       (if (and (number? v1) (number? v2))
                                           (num-compare v1 v2)
                                           (string-compare (string-downcase v1)
                                                           (string-downcase v2)))))))))

  (set-box! last-column-clicked-for-sorting-box column-clicked)

  (set-box! choices-box sorted-choices)

  (send-values-to-list-box sorted-choices)
  (void))

(define (concept-list parent
                      parent-search/isa-panel
                      parent-list-boxes-panel
                      label
                      name-string
                      isa-flag
                      choices
                      predicate-list-box-thunk
                      predicate-choices
                      edge-type
                      last-column-clicked-for-sorting-box
                      column-sort-order-vector
                      choices-box
                      convert-values-to-column-sorting-format
                      send-values-to-list-box)
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
  (define concept-listbox (new smart-column-width-list-box%
                               (label label)
                               (choices '())
                               (columns '("DB" "CID" "CUI" "Category" "Name"))
                               (parent parent-list-boxes-panel)
                               (style '(column-headers clickable-headers reorderable-headers extended))
                               (callback (lambda (self event)
                                           (define event-type (send event get-event-type))
                                           (cond
                                             [(eqv? event-type 'list-box-column)                                              
                                              (handle-sort-by-column-header-click
                                                    event
                                                    last-column-clicked-for-sorting-box
                                                    column-sort-order-vector
                                                    choices-box
                                                    convert-values-to-column-sorting-format
                                                    send-values-to-list-box)]
                                             [else
                                              (define selections (send self get-selections))
                                              (define selected-concepts (foldr (lambda (i l) (cons (list-ref (unbox choices) i) l)) '() selections))
                                              (when *verbose*
                                                (printf "selected concepts:\n~s\n" selected-concepts))
                                              (define concept-predicateo
                                                (case edge-type
                                                  [(in-edge)  object-predicateo]
                                                  [(out-edge) subject-predicateo]
                                                  [else       (error 'concept-listbox/predicates)]))
                                              (define predicates
                                                (sort (remove-duplicates
                                                       (time (run* (predicate)
                                                               (fresh (dbname pid c)
                                                                 (membero c selected-concepts)
                                                                 (concept-predicateo c `(,dbname ,pid . ,predicate))))))
                                                      string<?))
                                              (define (create-increase/decrease-syn-pred-list
                                                       syn-pred-prefix predicate-names selected-predicates)
                                                (let ((inter (sort (set-intersect predicate-names selected-predicates)
                                                                   string<?)))
                                                  (if (not (null? inter))
                                                      (let ((str (string-append syn-pred-prefix " (" (string-join inter ", ") ")")))
                                                        (let ((safe-string (~a str #:max-width MAX-CHAR-WIDTH #:limit-marker "...")))
                                                          (list safe-string))) 
                                                      '())))
                                              (define decreases-synthetic-predicate-string-list
                                                (create-increase/decrease-syn-pred-list
                                                 DECREASES_PREDICATE_PREFIX_STRING DECREASES_PREDICATE_NAMES predicates))
                                              (define increases-synthetic-predicate-string-list
                                                (create-increase/decrease-syn-pred-list
                                                 INCREASES_PREDICATE_PREFIX_STRING INCREASES_PREDICATE_NAMES predicates))
                                              (set! predicates (append
                                                                decreases-synthetic-predicate-string-list
                                                                increases-synthetic-predicate-string-list
                                                                predicates))
                                              (printf "predicates: ~s\n" predicates)
                                              (set-box! predicate-choices predicates)
                                              (send (predicate-list-box-thunk) set predicates)

                                              ;; unselect all items
                                              (for ([i (length predicates)])
                                                   (send (predicate-list-box-thunk) select i #f))
                                              ])))))

  (define (mk-run)
    (let* ((isa-count (if current-isa 50 0))  ;; Only grab the first 50.  50 should probably be a parameter.
           (subject? (case edge-type
                       [(out-edge) #t]
                       [(in-edge)  #f]))
           (object? (case edge-type
                      [(out-edge) #f]
                      [(in-edge)  #t]))
           (name-parts (split-name-string current-name))
           (ans (if (null? name-parts) '()
                  (begin (printf "searching for: ~s\n" current-name)
                         (time (find-concepts subject? object? isa-count #f name-parts))))))
      (set-box! choices ans)
      (send concept-listbox
            set
            (map (lambda (x)
                   (match x
                     [`(,dbname ,cid ,cui ,name (,catid . ,cat) ,props)
                       (~a dbname #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                 ans)
            (map (lambda (x)
                   (match x
                     [`(,dbname ,cid ,cui ,name (,catid . ,cat) ,props)
                       (format "~a" cid)]))
                 ans)
            (map (lambda (x)
                   (match x
                     [`(,dbname ,cid ,cui ,name (,catid . ,cat) ,props)
                       (format "~a" cui)]))
                 ans)
            (map (lambda (x)
                   (match x
                     [`(,dbname ,cid ,cui ,name (,catid . ,cat) ,props)
                       (~a `(,catid . ,cat) #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                 ans)
            (map (lambda (x)
                   (match x
                     [`(,dbname ,cid ,cui ,name (,catid . ,cat) ,props)
                       (~a name #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                 ans))
      ;; unselect all items
      (for ([i (length ans)])
           (send concept-listbox select i #f))))

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
  ;; (launch-gene-window)
  (launch-main-window))

(define (launch-main-window)
  (let ((frame (new frame%
                    (label MEDIKANREN_VERSION_STRING)
                    (width HORIZ-SIZE)
                    (height VERT-SIZE))))

    (define outer-vert-draggable-panel (new panel:vertical-dragable%
                                            (parent frame)
                                            (alignment '(left center))))
    
    (define upper-pane (new panel:vertical-dragable%
                            (parent outer-vert-draggable-panel)
                            (alignment '(left center))))

    (define lower-pane (new panel:vertical-dragable%
                            (parent outer-vert-draggable-panel)
                            (alignment '(left center))))
    
    (define go-callback
      (lambda (button event)
        (send running-status-description set-label "Running...")

        (define concept-1-selections (send concept-1-list-box get-selections))
        (define concept-2-selections (send concept-2-list-box get-selections))

        (define concept-1-selected-concepts
          (foldr (lambda (i l) (cons (list-ref (unbox *concept-1-choices*) i) l))
                 '()
                 concept-1-selections))
        (define concept-2-selected-concepts
          (foldr (lambda (i l) (cons (list-ref (unbox *concept-2-choices*) i) l))
                 '()
                 concept-2-selections))

        (printf "concept-1-selections: ~s\n" concept-1-selections)
        (displayln concept-1-selected-concepts)
        (printf "---------------------------------\n")
        (printf "concept-2-selections: ~s\n" concept-2-selections)
        (displayln concept-2-selected-concepts)
        (printf "---------------------------------\n")


        (define predicate-1-selections (send predicate-1-list-box get-selections))
        (define predicate-2-selections (send predicate-2-list-box get-selections))

        (define predicate-1-selected-predicates
          (foldr (lambda (i l) (cons (list-ref (unbox *predicate-1-choices*) i) l))
                 '()
                 predicate-1-selections))
        (define predicate-2-selected-predicates
          (foldr (lambda (i l) (cons (list-ref (unbox *predicate-2-choices*) i) l))
                 '()
                 predicate-2-selections))


        (printf "predicate-1-selections: ~s\n" predicate-1-selections)
        (displayln predicate-1-selected-predicates)
        (printf "---------------------------------\n")
        (printf "predicate-2-selections: ~s\n" predicate-2-selections)
        (displayln predicate-2-selected-predicates)
        (printf "---------------------------------\n")


        (find-X-concepts concept-1-selected-concepts
                         concept-2-selected-concepts
                         predicate-1-selected-predicates
                         predicate-2-selected-predicates
                         concept-X-list-box
                         running-status-description
                         full-path-list-box
                         subject-properties-list-box
                         edge-properties-list-box
                         object-properties-list-box
                         pubmed-list-box)

        ))

    (define concept-1-overall-pane (new vertical-pane%
                                        (parent upper-pane)
                                        (alignment '(left center))))
    
    (define concept-1-search/isa-panel (new panel:horizontal-dragable%
                                            (parent concept-1-overall-pane)
                                            (alignment '(left center))
                                            (stretchable-height #f)))
    (define concept-1-list-boxes-panel (new panel:horizontal-dragable%
                                            (parent concept-1-overall-pane)
                                            (alignment '(left center))))
    (define concept-1-list-box (concept-list concept-1-overall-pane
                                             concept-1-search/isa-panel
                                             concept-1-list-boxes-panel
                                             "Concept 1"
                                             *concept-1-name-string*
                                             *concept-1-isa-flag*
                                             *concept-1-choices*
                                             (lambda () predicate-1-list-box)
                                             *predicate-1-choices*
                                             'out-edge
                                             *last-concept-1-column-clicked-for-sorting*
                                             *concept-1-column-sort-order*
                                             *concept-1-choices*
                                             convert-concept-1/2-to-column-sorting-format
                                             (make-send-concepts-to-concept-1/2-list-box (lambda () concept-1-list-box))))
    (define predicate-1-list-box (new list-box%
                                      (label "Predicate 1")
                                      (choices (unbox *predicate-1-choices*))
                                      (columns '("Name"))
                                      (parent concept-1-list-boxes-panel)
                                      (style '(extended))
                                      (callback go-callback)))
    (define edge-description (new message%
                                  (parent concept-1-overall-pane)
                                  (label "Concept 1 -> Predicate 1 -> [X] -> Predicate 2 -> Concept 2")))

    (define concept-2-overall-pane (new vertical-pane%
                                        (parent upper-pane)
                                        (alignment '(left center))))
    
    (define concept-2-search/isa-panel (new panel:horizontal-dragable%
                                            (parent concept-2-overall-pane)
                                            (alignment '(left center))
                                            (stretchable-height #f)))
    (define concept-2-list-boxes-panel (new panel:horizontal-dragable%
                                            (parent concept-2-overall-pane)
                                            (alignment '(left center))))
    (define predicate-2-list-box (new list-box%
                                      (label "Predicate 2")
                                      (choices (unbox *predicate-2-choices*))
                                      (columns '("Name"))
                                      (parent concept-2-list-boxes-panel)
                                      (style '(extended))
                                      (callback go-callback)))
    (define concept-2-list-box (concept-list concept-2-overall-pane
                                             concept-2-search/isa-panel
                                             concept-2-list-boxes-panel
                                             "Concept 2"
                                             *concept-2-name-string*
                                             *concept-2-isa-flag*
                                             *concept-2-choices*
                                             (lambda () predicate-2-list-box)
                                             *predicate-2-choices*
                                             'in-edge
                                             *last-concept-2-column-clicked-for-sorting*
                                             *concept-2-column-sort-order*
                                             *concept-2-choices*
                                             convert-concept-1/2-to-column-sorting-format
                                             (make-send-concepts-to-concept-1/2-list-box (lambda () concept-2-list-box))))

    (define running-status-description (new message%
                                            (parent concept-2-overall-pane)
                                            (label "                                                                ")))

    (define concept-X-list-box (new smart-column-width-list-box%
                                    (label "X")
                                    (choices (unbox *concept-X-choices*))
                                    (columns '("DB" "CID" "CUI" "Category" "Name" "Max PubMed #" "Min PubMed #" "Path Length" "Path Confidence"))
                                    (parent lower-pane)
                                    (style '(column-headers clickable-headers reorderable-headers single))
                                    (callback (lambda (self event)
                                                (define event-type (send event get-event-type))
                                                (cond
                                                  [(eqv? event-type 'list-box-column)
                                                   (handle-sort-by-column-header-click
                                                    event
                                                    *last-concept-X-column-clicked-for-sorting*
                                                    *concept-X-column-sort-order*
                                                    *concept-X-choices*
                                                    convert-X-concept-to-column-sorting-format
                                                    (make-send-concepts-to-concept-X-list-box self))]
                                                  [(eqv? event-type 'list-box-dclick)
                                                   (printf "double-click!! copy name of the concept to the clipboard\n")
                                                   (define time-stamp (send event get-time-stamp))
                                                   (printf "time stamp: ~s\n" time-stamp)
                                                   (define concept-name
                                                     (let ((sel* (send concept-X-list-box get-selections)))
                                                       (if (= (length sel*) 1)
                                                           (let ((selected-X (list-ref (unbox *concept-X-choices*) (car sel*))))
                                                             (match selected-X
                                                               [`(,dbname ,cid ,cui ,name (,catid . ,cat) ,props ,max-pubmed-count ,min-pubmed-count ,path-count ,confidence)
                                                                name]
                                                               [else ""]))
                                                           "")))
                                                   (printf "concept name: ~s\n" concept-name)
                                                   (send the-clipboard set-clipboard-string concept-name time-stamp)]
                                                  [else
                                                   ;; empty the entries in the properties list-boxes
                                                   (send subject-properties-list-box set '() '())
                                                   (send edge-properties-list-box set '() '())
                                                   (send object-properties-list-box set '() '())

                                                   ;; empty the entries in the pubmed-list-box
                                                   (send pubmed-list-box set '())

                                                   (let ((sel* (send concept-X-list-box get-selections)))
                                                     (when (= (length sel*) 1)
                                                       (let ((selected-X (list-ref (unbox *concept-X-choices*) (car sel*))))
                                                         (let ((selected-X
                                                                (match selected-X
                                                                  [`(,dbname ,cid ,cui ,name (,catid . ,cat) ,props ,max-pubmed-count ,min-pubmed-count ,path-count ,confidence)
                                                                   `(,dbname ,cid ,cui ,name (,catid . ,cat) ,props)])))
                                                           (printf "selected ~s\n" selected-X)
                                                           (define concept-1* (unbox *solution-concept-1-choices*))
                                                           (define concept-2* (unbox *solution-concept-2-choices*))
                                                           (printf "concept-1* ~s\n" concept-1*)
                                                           (printf "concept-2* ~s\n" concept-2*)
                                                           (define predicate-1* (unbox *solution-predicate-1-choices*))
                                                           (define predicate-2* (unbox *solution-predicate-2-choices*))
                                                           (printf "predicate-1* ~s\n" predicate-1*)
                                                           (printf "predicate-2* ~s\n" predicate-2*)


                                                           (define atomic/synthetic-predicate-1* (split-atomic/synthetic-predicates predicate-1*))
                                                           (define atomic/synthetic-predicate-2* (split-atomic/synthetic-predicates predicate-2*))

                                                           (define atomic-predicate-1* (car atomic/synthetic-predicate-1*))
                                                           (define atomic-predicate-2* (car atomic/synthetic-predicate-2*))

                                                           (define synthetic-predicate-1* (cadr atomic/synthetic-predicate-1*))
                                                           (define synthetic-predicate-2* (cadr atomic/synthetic-predicate-2*))


                                                           (define paths '())


                                                           (cond
                                                             [(and
                                                               (null?
                                                                (split-name-string
                                                                 (unbox *solution-concept-1-name-string*)))
                                                               (null?
                                                                (split-name-string
                                                                 (unbox *solution-concept-2-name-string*))))

                                                              (set! paths '())]
                                                             [(null? (split-name-string (unbox *solution-concept-1-name-string*)))

                                                              (set! paths '())
                                                              ;; run synthetic queries here
                                                              (set! paths
                                                                    (remove-duplicates
                                                                     (append paths
                                                                             (run* (q)
                                                                               (fresh (e dbname eid x o pid pred eprops)
                                                                                 (== (list `(,dbname ,eid ,x ,o (,pid . ,pred) ,eprops)) q)
                                                                                 (== `(,dbname . ,x) selected-X)
                                                                                 (== `(,dbname ,eid ,x ,o (,pid . ,pred) ,eprops) e)
                                                                                 (membero `(,dbname . ,o) concept-2*)
                                                                                 (membero pred atomic-predicate-2*)
                                                                                 (edgeo e))))))]
                                                             [(null? (split-name-string (unbox *solution-concept-2-name-string*)))

                                                              (set! paths '())
                                                              ;; run synthetic queries here
                                                              (set! paths
                                                                    (remove-duplicates
                                                                     (append paths
                                                                             (run* (q)
                                                                               (fresh (e dbname eid s x pid pred eprops)
                                                                                 (== (list `(,dbname ,eid ,s ,x (,pid . ,pred) ,eprops)) q)
                                                                                 (== `(,dbname . ,x) selected-X)
                                                                                 (== `(,dbname ,eid ,s ,x (,pid . ,pred) ,eprops) e)
                                                                                 (membero `(,dbname . ,s) concept-1*)
                                                                                 (membero pred atomic-predicate-1*)
                                                                                 (edgeo e))))))]
                                                             [else
                                                              (set! paths '())
                                                              ;; run synthetic queries here
                                                              (set! paths
                                                                    (remove-duplicates
                                                                     (append paths
                                                                             (run* (q)
                                                                               (fresh (e1 e2 dbname eid1 eid2 s x o pid1 pid2 p1 p2 eprops1 eprops2)
                                                                                 (== `(,dbname . ,x) selected-X)
                                                                                 (== (list
                                                                                      `(,dbname ,eid1 ,s ,x (,pid1 . ,p1) ,eprops1)
                                                                                      `(,dbname ,eid2 ,x ,o (,pid2 . ,p2) ,eprops2))
                                                                                     q)
                                                                                 (== `(,dbname ,eid1 ,s ,x (,pid1 . ,p1) ,eprops1) e1)
                                                                                 (== `(,dbname ,eid2 ,x ,o (,pid2 . ,p2) ,eprops2) e2)
                                                                                 (membero `(,dbname . ,s) concept-1*)
                                                                                 (membero `(,dbname . ,o) concept-2*)
                                                                                 (membero p1 atomic-predicate-1*)
                                                                                 (membero p2 atomic-predicate-2*)
                                                                                 (edgeo e1)
                                                                                 (edgeo e2))))))])

                                                           (printf "paths: ~s\n" paths)
                                                           (newline)

                                                           ;; This sorting affects the order of the "Path" list
                                                           ;; for the selected concept.
                                                           (set! paths (sort-paths paths))
                                                           
                                                           (define flattened-paths
                                                             (let ((ls (foldr
                                                                        (lambda (p l)
                                                                          (cons
                                                                           'path-separator
                                                                           (append (reverse p) l)))
                                                                        '()
                                                                        paths)))
                                                               (if (null? ls)
                                                                   ;; ls should never be null!
                                                                   '()
                                                                   (reverse (cdr ls)))))



                                                           (define full-path-dbname-list
                                                             (map (lambda (x)
                                                                    (match x
                                                                      ['path-separator "----"]
                                                                      [`(,dbname ,eid ,subj ,obj (,pid . ,pred) ,eprops)
                                                                       (~a dbname)]))
                                                                  flattened-paths))

                                                           (define full-path-eid-list
                                                             (map (lambda (x)
                                                                    (match x
                                                                      ['path-separator "----"]
                                                                      [`(,dbname ,eid ,subj ,obj (,pid . ,pred) ,eprops)
                                                                       (~a eid)]))
                                                                  flattened-paths))

                                                           (define full-path-subj-list
                                                             (map (lambda (x)
                                                                    (match x
                                                                      ['path-separator "----"]
                                                                      [`(,dbname ,eid ,subj ,obj (,pid . ,pred) ,eprops)
                                                                       (~a subj #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                                                                  flattened-paths))

                                                           (define full-path-pred-list
                                                             (map (lambda (x)
                                                                    (match x
                                                                      ['path-separator "----"]
                                                                      [`(,dbname ,eid ,subj ,obj (,pid . ,pred) ,eprops)
                                                                       (~a `(,pid . ,pred) #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                                                                  flattened-paths))

                                                           (define full-path-obj-list
                                                             (map (lambda (x)
                                                                    (match x
                                                                      ['path-separator "----"]
                                                                      [`(,dbname ,eid ,subj ,obj (,pid . ,pred) ,eprops)
                                                                       (~a obj #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                                                                  flattened-paths))

                                                           (define full-path-subj-cat-list
                                                             (map (lambda (x)
                                                                    (match x
                                                                      ['path-separator "----"]
                                                                      [`(,dbname ,eid (,cid ,cui ,name (,catid . ,cat) ,props) ,obj (,pid . ,pred) ,eprops)
                                                                       (~a `(,catid . ,cat) #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                                                                  flattened-paths))

                                                           (define full-path-obj-cat-list
                                                             (map (lambda (x)
                                                                    (match x
                                                                      ['path-separator "----"]
                                                                      [`(,dbname ,eid ,subj (,cid ,cui ,name (,catid . ,cat) ,props) (,pid . ,pred) ,eprops)
                                                                       (~a `(,catid . ,cat) #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                                                                  flattened-paths))

                                                           (define full-path-PubMed-count-list
                                                             (map (lambda (x)
                                                                    (match x
                                                                      ['path-separator "----"]
                                                                      [`(,dbname ,eid ,subj (,cid ,cui ,name (,catid . ,cat) ,props) (,pid . ,pred) ,eprops)
                                                                       (~a (length (pubmed-URLs-from-edge x)))]))
                                                                  flattened-paths))

                                                           (send full-path-list-box
                                                                 set
                                                                 full-path-dbname-list
                                                                 full-path-eid-list
                                                                 full-path-subj-list
                                                                 full-path-pred-list
                                                                 full-path-obj-list
                                                                 full-path-subj-cat-list
                                                                 full-path-obj-cat-list
                                                                 full-path-PubMed-count-list
                                                                 )

                                                           (set-box! *full-path-choices* flattened-paths)

                                                           ;; unselect all items
                                                           (for ([i (length flattened-paths)])
                                                                (send full-path-list-box select i #f))

                                                           ;; select first item
                                                           (send full-path-list-box select 0 #t)
                                                           )))
                                                     (void))])))))

    (define full-path-list-box (new smart-column-width-list-box%
                                    (label "Paths")
                                    (choices (unbox *full-path-choices*))
                                    (columns '("DB" "EID" "Subject" "Predicate" "Object" "Subj Cat" "Obj Cat" "PubMed #"))
                                    (parent lower-pane)
                                    (style '(column-headers reorderable-headers extended))
                                    (callback (lambda (self event)
                                                (when *verbose*
                                                  (printf "(unbox *full-path-choices*):\n~s\n" (unbox *full-path-choices*)))
                                                (define selections (send self get-selections))
                                                (when *verbose*
                                                  (printf "selection for full path:\n~s\n" selections))
                                                (define selected-full-paths
                                                  (foldr (lambda (i l) (cons (list-ref (unbox *full-path-choices*) i) l))
                                                         '()
                                                         selections))
                                                (for-each
                                                    (lambda (x)
                                                      (match x
                                                        ['path-separator
                                                         (send subject-properties-list-box set '() '())
                                                         (send edge-properties-list-box set '() '())
                                                         (send object-properties-list-box set '() '())]
                                                        [`(,dbname ,eid
                                                                   (,scid ,scui ,sname (,scatid . ,scat) ,sprops)
                                                                   (,ocid ,ocui ,oname (,ocatid . ,ocat) ,oprops)
                                                                   ,p
                                                                   ,eprops)
                                                         (define (set-properties-list-box prop-list-box props)
                                                           (send prop-list-box
                                                                 set
                                                                 (map
                                                                  (lambda (p)
                                                                    (~a (car p)
                                                                        #:max-width
                                                                        MAX-CHAR-WIDTH
                                                                        #:limit-marker
                                                                        "..."))
                                                                  props)
                                                                 (map
                                                                  (lambda (p)
                                                                    (~a (cdr p)
                                                                        #:max-width
                                                                        MAX-CHAR-WIDTH
                                                                        #:limit-marker
                                                                        "..."))
                                                                  props)))
                                                         (set-properties-list-box subject-properties-list-box sprops)
                                                         (set-properties-list-box edge-properties-list-box eprops)
                                                         (set-properties-list-box object-properties-list-box oprops)]))
                                                    selected-full-paths)
                                                (for-each
                                                  (lambda (edge)
                                                    (let ((URLs (pubmed-URLs-from-edge edge)))
                                                      (set-box! *pubmed-choices* URLs)
                                                      (send pubmed-list-box set URLs)))
                                                  selected-full-paths)
                                                (when *verbose*
                                                  (printf "selected full path:\n")
                                                  (for-each
                                                    (lambda (x)
                                                      (match x
                                                        ['path-separator
                                                         (printf "-----------------------\n")]
                                                        [`(,dbname ,eid ,subj ,obj ,p ,eprops)
                                                         (pretty-print `(,dbname ,eid ,subj ,obj ,p ,eprops))]))
                                                    selected-full-paths))
                                                ))))

    (define properties/pubmed-panel (new panel:horizontal-dragable%
                                         (parent lower-pane)
                                         (alignment '(left center))
                                         (stretchable-height #t)))

    (define subject-properties-list-box (new smart-column-width-list-box%
                                             (label "Subject")
                                             (choices '())
                                             (columns '("Property" "Value"))
                                             (parent properties/pubmed-panel)
                                             (style '(column-headers reorderable-headers extended))
                                             (callback (lambda (self event)
                                                         (void)))))
    
    (define edge-properties-list-box (new smart-column-width-list-box%
                                     (label "Edge")
                                     (choices '())
                                     (columns '("Property" "Value"))
                                     (parent properties/pubmed-panel)
                                     (style '(column-headers reorderable-headers extended))
                                     (callback (lambda (self event)
                                                 (void)))))

    (define object-properties-list-box (new smart-column-width-list-box%
                                            (label "Object")
                                            (choices '())
                                            (columns '("Property" "Value"))
                                            (parent properties/pubmed-panel)
                                            (style '(column-headers reorderable-headers extended))
                                            (callback (lambda (self event)
                                                        (void)))))
    
    (define pubmed-list-box (new list-box%
                                 (label "Pubmed")
                                 (choices (unbox *pubmed-choices*))
                                 (columns '("URL"))
                                 (parent properties/pubmed-panel)
                                 (style '(column-headers reorderable-headers single))
                                 (callback (lambda (self event)                                        
                                             (define event-type (send event get-event-type))
                                             (define selections (send self get-selections))
                                             (define selected-pubmeds
                                               (foldr (lambda (i l) (cons (list-ref (unbox *pubmed-choices*) i) l))
                                                      '()
                                                      selections))
                                             (for-each
                                               (lambda (url)
                                                 (printf "url: ~s\n" url)
                                                 (when (eqv? event-type 'list-box-dclick)
                                                   ;; if the user double-clicked on the URL, open it in a web browser
                                                   (send-url url)))
                                               selected-pubmeds)))))
   
    ;; trigger reflowing of object sizes
    (send frame reflow-container)
    
    (set-default-column-widths concept-1-list-box)
    (set-default-column-widths concept-2-list-box)
    (set-default-column-widths concept-X-list-box)
    (set-default-column-widths full-path-list-box)
    (set-default-column-widths edge-properties-list-box)
    (set-default-column-widths edge-properties-list-box)
    (set-default-column-widths object-properties-list-box)
        
    (send frame show #t)))

(define (launch-gene-window)
  (let ((frame (new frame%
                    (label "Gene Explorer")
                    (width HORIZ-SIZE)
                    (height VERT-SIZE))))

    (define gene-name-field (new text-field%
                                 (label "Gene Name")
                                 (parent frame)
                                 (init-value "")
                                 (callback (lambda (self event)
                                             (void)))))

    (define gene-listbox (new smart-column-width-list-box%
                              (label "Gene")
                              (choices '())
                              (columns '("DB" "CID" "CUI" "Category" "Name"))
                              (parent frame)
                              (style '(column-headers reorderable-headers extended))
                              (callback (lambda (self event)
                                          (void)))))


    (define current-gene-name "")

    (send frame show #t)
    ))


(define (split-atomic/synthetic-predicates predicate*)

  (define atomic-predicate* predicate*)
  (define synthetic-predicate* '())

  (set! synthetic-predicate* (filter (lambda (pred)
                                       (memf (lambda (syn-prefix) (string-prefix? pred syn-prefix))
                                             SYNTHETIC_PREDICATE_PREFIXES))
                                     atomic-predicate*))

  (when (memf (lambda (pred) (string-prefix? pred DECREASES_PREDICATE_PREFIX_STRING))
              atomic-predicate*)
    (set! atomic-predicate* (append DECREASES_PREDICATE_NAMES atomic-predicate*))
    (set! synthetic-predicate* (remove DECREASES_PREDICATE_STRING synthetic-predicate*)))

  (when (memf (lambda (pred) (string-prefix? pred INCREASES_PREDICATE_PREFIX_STRING))
              atomic-predicate*)
    (set! atomic-predicate* (append INCREASES_PREDICATE_NAMES atomic-predicate*))
    (set! synthetic-predicate* (remove INCREASES_PREDICATE_STRING synthetic-predicate*)))

  (set! atomic-predicate* (filter (lambda (pred)
                                    (not (memf (lambda (syn-prefix) (string-prefix? pred syn-prefix))
                                               SYNTHETIC_PREDICATE_PREFIXES))) 
                                  atomic-predicate*))

  (set! atomic-predicate* (remove-duplicates atomic-predicate*))

  (printf "atomic-predicate*: ~s\n" atomic-predicate*)
  (printf "synthetic-predicate*: ~s\n" synthetic-predicate*)

  (list atomic-predicate* synthetic-predicate*)
  )


(define (find-X-concepts concept-1* concept-2* predicate-1* predicate-2* concept-X-list-box running-status-description full-path-list-box subject-properties-list-box edge-properties-list-box object-properties-list-box pubmed-list-box)

  (define start-time (current-milliseconds))

  (printf "\nfinding concepts X for which\n[C1] -> P1 -> [X] -> P2 -> [C2]\n")
  (printf "=============================\n")

  (define atomic/synthetic-predicate-1* (split-atomic/synthetic-predicates predicate-1*))
  (define atomic/synthetic-predicate-2* (split-atomic/synthetic-predicates predicate-2*))

  (define atomic-predicate-1* (car atomic/synthetic-predicate-1*))
  (define atomic-predicate-2* (car atomic/synthetic-predicate-2*))

  (define synthetic-predicate-1* (cadr atomic/synthetic-predicate-1*))
  (define synthetic-predicate-2* (cadr atomic/synthetic-predicate-2*))

  (define all-X-concepts-with-edges '())

  (printf "atomic/synthetic-predicate-1*: ~s\n" atomic/synthetic-predicate-1*)
  (printf "atomic/synthetic-predicate-2*: ~s\n" atomic/synthetic-predicate-2*)
  (newline)
  (printf "atomic-predicate-1*: ~s\n" atomic-predicate-1*)
  (printf "atomic-predicate-2*: ~s\n" atomic-predicate-2*)
  (newline)
  (printf "synthetic-predicate-1*: ~s\n" synthetic-predicate-1*)
  (printf "synthetic-predicate-2*: ~s\n" synthetic-predicate-2*)
  (newline)

  (cond
    [(and
      (null?
       (split-name-string
        (unbox *concept-1-name-string*)))
      (null?
       (split-name-string
        (unbox *concept-2-name-string*))))

     (set! all-X-concepts-with-edges '())]
    [(null? (split-name-string (unbox *concept-1-name-string*)))

     (set! all-X-concepts-with-edges '())
     (set! all-X-concepts-with-edges
           (remove-duplicates
            (append all-X-concepts-with-edges
                    (run* (q)
                      (fresh (dbname eid s o pid pred eprops e)
                        ;; TODO FIXME -- epropos may contain pubmed ids--how to extract it, or other evidence?
                        (== (list dbname s (list eprops) (list e)) q)
                        (== `(,dbname ,eid ,s ,o (,pid . ,pred) ,eprops) e)
                        (membero `(,dbname . ,o) concept-2*)
                        (membero pred atomic-predicate-2*)
                        (edgeo e))))))]
    [(null? (split-name-string (unbox *concept-2-name-string*)))

     (set! all-X-concepts-with-edges '())
     ;; run synthetic queries here
     (set! all-X-concepts-with-edges
           (remove-duplicates
            (append all-X-concepts-with-edges
                    (run* (q)
                      (fresh (dbname eid s o pid pred eprops e)
                        ;; TODO FIXME -- epropos may contain pubmed ids--how to extract it, or other evidence?
                        (== (list dbname o (list eprops) (list e)) q)
                        (== `(,dbname ,eid ,s ,o (,pid . ,pred) ,eprops) e)
                        (membero `(,dbname . ,s) concept-1*)
                        (membero pred atomic-predicate-1*)
                        (edgeo e))))))]
    [else
     (set! all-X-concepts-with-edges '())
     ;; run synthetic queries here
     (set! all-X-concepts-with-edges
           (remove-duplicates
            (append all-X-concepts-with-edges
                    (run* (q)
                      (fresh (dbname eid1 eid2 s m o pid1 pid2 p1 p2 eprops1 eprops2 e1 e2)
                        (== (list dbname m (list eprops1 eprops2) (list e1 e2)) q)
                        (== `(,dbname ,eid1 ,s ,m (,pid1 . ,p1) ,eprops1) e1)
                        (== `(,dbname ,eid2 ,m ,o (,pid2 . ,p2) ,eprops2) e2)
                        (membero `(,dbname . ,s) concept-1*)
                        (membero `(,dbname . ,o) concept-2*)
                        (membero p1 atomic-predicate-1*)
                        (membero p2 atomic-predicate-2*)
                        (edgeo e1)
                        (edgeo e2))))))])

  (define end-time (current-milliseconds))

  (define elapsed-time (- end-time start-time))

  (printf "elapsed query time: ~s seconds\n" (/ elapsed-time 1000.0))
  (printf "=============================\n")

  ;; This sorting affects order of appearance in the "X" concept list
  (set! all-X-concepts-with-edges
    (sort
      all-X-concepts-with-edges
      (lambda (c1 c2)
        (match (list c1 c2)
          [`((,_ ,_ ,_ ,e1*) (,_ ,_ ,_ ,e2*))
           (not (path-confidence<? e1* e2*))]))))

  (define all-X-concepts '())
  (set! all-X-concepts
        (let loop ([ls all-X-concepts-with-edges])
          (cond
            [(null? ls) '()]
            [else
             (match (car ls)
               [`(,dbname (,cid ,cui ,name (,catid . ,cat) ,props) ,whatever ,e*)
                (let ((pubmed-count* (map pubmed-count e*)))
                  (let ((max-pubmed-count (apply max pubmed-count*))
                        (min-pubmed-count (apply min pubmed-count*))
                        (path-length (length pubmed-count*))
                        (confidence (path-confidence e*)))
                    (cons `(,dbname (,cid ,cui ,name (,catid . ,cat) ,props ,max-pubmed-count ,min-pubmed-count ,path-length ,confidence) ,whatever ,e*)
                          (loop (remf* (lambda (x)
                                         (match x
                                           [`(,dbname-x (,cid-x ,cui-x ,name-x (,catid-x . ,cat-x) ,props-x) . ,rest-x)
                                            (and (equal? dbname dbname-x)
                                                 (equal? cid cid-x)
                                                 (equal? cui cui-x))]))
                                       (cdr ls))))))])])))
  (set! all-X-concepts (map (lambda (e) (cons (car e) (cadr e))) all-X-concepts))

  (newline)
  (printf "========== begin query results =============\n")
  (newline)

  (printf "Query end date/time:\n~a\n" (date->string (seconds->date (current-seconds)) #t))
  (newline)


  (define number-Xs-found (length all-X-concepts))
  (define query-seconds (/ elapsed-time 1000.0))
  (define query-time-format-string "Found ~s X's after ~s seconds")
  (send running-status-description set-label (format query-time-format-string number-Xs-found query-seconds))
  (printf query-time-format-string number-Xs-found query-seconds)
  (newline)
  (newline)


  (set-box! *concept-X-choices* all-X-concepts)

  (set-box! *solution-concept-1-name-string* (unbox *concept-1-name-string*))
  (set-box! *solution-concept-2-name-string* (unbox *concept-2-name-string*))

  (set-box! *solution-concept-1-isa-flag* (unbox *concept-1-isa-flag*))
  (set-box! *solution-concept-2-isa-flag* (unbox *concept-2-isa-flag*))

  (set-box! *solution-concept-1-choices* concept-1*)
  (set-box! *solution-concept-2-choices* concept-2*)
  (set-box! *solution-predicate-1-choices* predicate-1*)
  (set-box! *solution-predicate-2-choices* predicate-2*)

  (printf "*solution-concept-1-name-string*:\n~s\n" (unbox *solution-concept-1-name-string*))
  (printf "*solution-concept-1-isa-flag*:\n~s\n" (unbox *solution-concept-1-isa-flag*))
  (printf "*solution-concept-1-choices*:\n")
  (pretty-print (unbox *solution-concept-1-choices*))
  (printf "*solution-predicate-1-choices*:\n")
  (pretty-print (unbox *solution-predicate-1-choices*))
  (newline)

  (printf "*solution-concept-2-name-string*:\n~s\n" (unbox *solution-concept-2-name-string*))
  (printf "*solution-concept-2-isa-flag*:\n~s\n" (unbox *solution-concept-2-isa-flag*))
  (printf "*solution-concept-2-choices*:\n")
  (pretty-print (unbox *solution-concept-2-choices*))
  (printf "*solution-predicate-2-choices*:\n")
  (pretty-print (unbox *solution-predicate-2-choices*))
  (newline)

  (define pretty-print-X-concepts-with-edges
    (lambda (file-name pretty-printer print-basic-header print-low-level-query-information X-concepts-with-edges)
      (with-output-to-file
          file-name
          (lambda ()
            (when print-basic-header
              (printf ";; mediKanren query output\n")
              (printf ";; ~a\n" MEDIKANREN_VERSION_STRING)
              (printf ";; \n")
              (let ((local-date (seconds->date (current-seconds))))
                (printf ";; Query run at date/time: ~s ~s ~s  ~s:~s:~s   (Year Month Day  Hour:Minute:Second)\n"
                        (date-year local-date)
                        (date-month local-date)
                        (date-day local-date)
                        (date-hour local-date)
                        (date-minute local-date)
                        (date-second local-date)))
              (printf ";; \n"))
            (when print-low-level-query-information
              (printf ";; ===================================================\n")
              (printf ";; Low-level query information from the mediKanren GUI\n")
              (printf ";; ===================================================\n")
              (printf ";; \n")
              (printf ";; *concept-1-name-string*: ~s\n" (unbox *concept-1-name-string*))
              (printf ";; *solution-concept-1-name-string*: ~s\n" (unbox *solution-concept-1-name-string*))
              (printf ";; *solution-concept-1-isa-flag*: ~s\n" (unbox *solution-concept-1-isa-flag*))
              (printf ";; *solution-concept-1-choices*:\n")
              (printf "#|\n")
              (pretty-print (unbox *solution-concept-1-choices*))
              (printf "|#\n")
              (printf ";; atomic/synthetic-predicate-1*: ~s\n" atomic/synthetic-predicate-1*)
              (printf ";; atomic-predicate-1*: ~s\n" atomic-predicate-1*)
              (printf ";; synthetic-predicate-1*: ~s\n" synthetic-predicate-1*)
              (printf ";; *solution-predicate-1-choices*:\n")
              (printf "#|\n")
              (pretty-print (unbox *solution-predicate-1-choices*))
              (printf "|#\n")
              (printf ";; \n")
              (printf ";; \n")
              (printf ";; \n")
              (printf ";; atomic/synthetic-predicate-2*: ~s\n" atomic/synthetic-predicate-2*)
              (printf ";; atomic-predicate-2*: ~s\n" atomic-predicate-2*)
              (printf ";; synthetic-predicate-2*: ~s\n" synthetic-predicate-2*)
              (printf ";; *solution-predicate-2-choices*:\n")
              (printf "#|\n")
              (pretty-print (unbox *solution-predicate-2-choices*))
              (printf "|#\n")
              (printf ";; *concept-2-name-string*: ~s\n" (unbox *concept-2-name-string*))
              (printf ";; *solution-concept-2-name-string*: ~s\n" (unbox *solution-concept-2-name-string*))
              (printf ";; *solution-concept-2-isa-flag*: ~s\n" (unbox *solution-concept-2-isa-flag*))
              (printf ";; *solution-concept-2-choices*:\n")
              (printf "#|\n")
              (pretty-print (unbox *solution-concept-2-choices*))
              (printf "|#\n")
              (printf "\n"))
            (when print-basic-header
              (printf ";; ======================================\n")
              (printf ";; Query results (list of complete edges)\n")
              (printf ";; ======================================\n"))
            (pretty-printer X-concepts-with-edges))
          #:mode 'text
          #:exists QUERY_RESULTS_FILE_MODE)))

  (define human-friendly-pretty-print-X-concepts-with-edges
    (lambda (X-concepts-with-edges)
      (for-each
        (lambda (entry index)
          (match entry
            [`(,dbname
                ,s
                ,eprops*
                ,edges)
              (printf "*** Edge group:")
              (for-each
                (lambda (edge)
                  (match edge
                    [`(,dbname
                       ,eid
                       (,scid ,scui ,sname (,scatid . ,scat) . ,sprops)
                       (,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops)
                       (,pid . ,pred) ,eprops)
                     (let ((pubmed* (pubmed-URLs-from-edge edge)))
                       (printf "\n~s\t~s\t~s\t~s\t~s\t~s\t~s\t~s PubMed Entries\n~s\n" index dbname sname scat pred oname ocat (length pubmed*) pubmed*))]))
                edges)
              (printf "***\n\n") ]))
        X-concepts-with-edges
        (iota (length X-concepts-with-edges) 1))))

  (define spreadsheet-friendly-pretty-print-X-concepts-with-edges
    (lambda (X-concepts-with-edges)
      (printf "~a\t~a\t~a\t~a\t~a\t~a\t~a\n"
              "Subject Category"
              "Subject Name"
              
              "Predicate"
              
              "Object Category"
              "Object Name"
              
              "PubMed URL"
              
              "DB Name")
      (for-each
        (lambda (entry index)
          (match entry
            [`(,dbname
                ,s
                ,eprops*
                ,edges)
              (for-each
                (lambda (edge)
                  (match edge
                    [`(,dbname
                       ,eid
                       (,scid ,scui ,sname (,scatid . ,scat) . ,sprops)
                       (,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops)
                       (,pid . ,pred) ,eprops)
                     (let ((pubmed* (pubmed-URLs-from-edge edge)))
                       (for-each
                         (lambda (pubmed)
                           (printf "~a\t~a\t~a\t~a\t~a\t~a\t~a\n"
                                   scat
                                   sname

                                   pred

                                   ocat
                                   oname                                   

                                   pubmed
                                   
                                   dbname))
                         pubmed*))]))
                edges)]))
        X-concepts-with-edges
        (iota (length X-concepts-with-edges) 1))))

  (when WRITE_QUERY_RESULTS_TO_FILE

    (printf "saving all-X-concepts-with-edges to '~a' file...\n"
            QUERY_RESULTS_FILE_NAME)
    (pretty-print-X-concepts-with-edges
      QUERY_RESULTS_FILE_NAME
      pretty-print
      #t ;; print-basic-header flag
      #t ;; print-low-level-query-information flag
      all-X-concepts-with-edges)
    (printf "saved all-X-concepts-with-edges to '~a' file\n"
            QUERY_RESULTS_FILE_NAME)

    (printf "saving human-friendly version of all-X-concepts-with-edges to '~a' file...\n"
            HUMAN_FRIENDLY_QUERY_RESULTS_FILE_NAME)
    (pretty-print-X-concepts-with-edges
      HUMAN_FRIENDLY_QUERY_RESULTS_FILE_NAME
      human-friendly-pretty-print-X-concepts-with-edges
      #t ;; print-basic-header flag
      #f ;; print-low-level-query-information flag
      all-X-concepts-with-edges)
    (printf "saved human-friendly version of all-X-concepts-with-edges to '~a' file\n"
            HUMAN_FRIENDLY_QUERY_RESULTS_FILE_NAME)

    (printf "saving spreadsheet-friendly version of all-X-concepts-with-edges to '~a' file...\n"
            SPREADSHEET_FRIENDLY_QUERY_RESULTS_FILE_NAME)
    (pretty-print-X-concepts-with-edges
      SPREADSHEET_FRIENDLY_QUERY_RESULTS_FILE_NAME
      spreadsheet-friendly-pretty-print-X-concepts-with-edges
      #f ;; print-basic-header flag
      #f ;; print-low-level-query-information flag
      all-X-concepts-with-edges)
    (printf "saved spreadsheet-friendly version of all-X-concepts-with-edges to '~a' file\n"
            SPREADSHEET_FRIENDLY_QUERY_RESULTS_FILE_NAME)

    )

  (printf "========== end query results =============\n")

  ((make-send-concepts-to-concept-X-list-box concept-X-list-box) all-X-concepts)

  ;; unselect all items
  (for ([i (length all-X-concepts)])
       (send concept-X-list-box select i #f))

  ;; empty the entries in the full-path-list-box
  (send full-path-list-box set '() '() '() '() '() '() '() '())

  ;; empty the entries in the properties list-boxes
  (send subject-properties-list-box set '() '())
  (send edge-properties-list-box set '() '())
  (send object-properties-list-box set '() '())

  ;; empty the entries in the pubmed-list-box
  (send pubmed-list-box set '())

  )

(displayln
  "Launching GUI")

(launch-gui)
