#|

WEB: mediKanren 2 Explorer TODO:

* fix sorting by CURIE, so that DOID:26 appears before DOID:2531

* fully implement concept normalization checkbox
* fully implement lightweight reasoning checkbox
* implement smarter copy/paste
* make the interface faster/more responsive

|#


#lang racket

(require
 "common.rkt"
 "synonyms.rkt"
 "string-search.rkt"
 ;; (prefix-in semmed: "db/semmed.rkt")
 (prefix-in rtx:    "db/rtx2-biolink_2_1_2021_07_28.rkt")
 (prefix-in kgx:    "db/kgx-synonym.rkt")
  json
  racket/sandbox
  racket/gui/base
  framework
  racket/engine
  racket/date
  racket/string
  net/sendurl
  (except-in racket/match ==)
  (only-in srfi/1 iota))

#|
(require "base.rkt"
         (prefix-in semmed: "db/semmed.rkt")
         (prefix-in rtx:    "db/rtx2-20210204.rkt")
         (prefix-in kgx:    "db/kgx-synonym.rkt"))
|#
#|
(require
 "common.rkt"
 "synonyms.rkt"
  racket/sandbox
  racket/gui/base
  framework
  racket/engine
  racket/date
  racket/string
  net/sendurl
  (except-in racket/match ==)
  (only-in srfi/1 iota))
|#

#|
Choice 1:
;;; find-ids-named
;; Consult the string index associated with rel to find the ids
;; in rel with a (name) string matching every substr in substrs,
;; according to options stsopt.  If the associated string index
;; has not been previously prepared, fail.
(define (find-ids-named rel substrs (stsopt stsopt-default))
(edited)

Choice 2:
;; Consult the string index associated with rel to find the concepts
;; in rel with a (name) string matching every substr in substrs,
;; according to options stsopt.  If the associated string index
;; has not been previously prepared, fail.
(define (find-concepts-named rel substrs (stsopt stsopt-default))
|#


(provide
  launch-gui)

(define MEDIKANREN_VERSION_STRING "mediKanren 2 Explorer 0.1.0")

(displayln "Starting mediKanren 2 Explorer...")
(newline)
(displayln "**************************************************")
(displayln "*** mediKanren 2 is for research purposes only ***")
(displayln "**************************************************")
(newline)
(displayln MEDIKANREN_VERSION_STRING)

;;; Query save file settings
(define WRITE_QUERY_RESULTS_TO_FILE            #f)
(define QUERY_RESULTS_FILE_NAME                "last.sx")
(define HUMAN_FRIENDLY_QUERY_RESULTS_FILE_NAME "last.txt")
(define SPREADSHEET_FRIENDLY_QUERY_RESULTS_FILE_NAME "last.tsv")
(define QUERY_RESULTS_FILE_MODE                'replace)

;;; Initial window size
(define HORIZ-SIZE 800)
(define VERT-SIZE 400)

;;; Decreases/increases predicate names
(define DECREASES_PREDICATE_NAMES
  (list
   "biolink:ameliorates"
   "biolink:approved_to_treat"
   "biolink:decreases_abundance_of"
   "biolink:decreases_activity_of"
   "biolink:decreases_expression_of"
   "biolink:decreases_synthesis_of"
   "biolink:decreases_transport_of"
   "biolink:decreases_uptake_of"
   "biolink:disrupts"
   "biolink:entity_negatively_regulates_entity"
   "biolink:prevents"
   "biolink:process_negatively_regulates_process"
   "biolink:treats"))
(define INCREASES_PREDICATE_NAMES
  (list
   "biolink:causes"
   "biolink:causes_adverse_event"
   "biolink:condition_associated_with_gene"
   "biolink:contributes_to"
   "biolink:decreases_degradation_of"
   "biolink:enables"
   "biolink:entity_positively_regulates_entity"
   "biolink:exacerbates"
   "biolink:gene_associated_with_condition"
   "biolink:increases_abundance_of"
   "biolink:increases_activity_of"
   "biolink:increases_expression_of"
   "biolink:increases_synthesis_of"
   "biolink:increases_transport_of"
   "biolink:process_positively_regulates_process"
   "biolink:produces"))




(define (curie-string str)
  (let ((cs (regexp-match* #px"^[\\s]*(([^\\s]+:[^\\s]*)|(:[^\\s]+))[\\s]*$" str #:match-select cadr)))
    (if (null? cs)
        #f
        (car cs))))


(define (sort-paths paths)
  (printf "sort-paths -- IMPLEMENT ME!\n")
  paths)

(define (path-confidence . args)
  (printf "path-confidence -- IMPLEMENT ME!\n")
  'todo)

(define (path-confidence<? . args)
  (printf "path-confidence<? -- IMPLEMENT ME!\n")
  'todo)

(define (get-pred-names e*)
  (let loop ([e* e*]
             [pred-names '()])
    (cond
      [(null? e*) pred-names]
      [else
       (let ((edge (car e*))
             (rest (cdr e*)))
         (match edge
           ['path-separator
            (loop rest pred-names)]
           [`(,dbname ,eid ,subj ,obj (,pid . ,p-name) ,eprops)
            (loop rest (if (member p-name pred-names)
                           pred-names
                           (cons p-name pred-names)))]
           [else (error 'get-pred-names (format "unmatched edge ~s\n" edge))]))])))

(define (pubmed-count e)
  (length (pubmed-ids-from-edge e)))

(define (pubmed-ids-from-edge-props eprops)
  (cond
    [(assoc "publications" eprops)
     => (lambda (pr)
          (define pubs (cdr pr))
          (let ((pubmed-ids (if (not (string? pubs))
                                '()
                                (regexp-match* #rx"PMID:([0-9]+)" pubs #:match-select cadr))))
            pubmed-ids))]
    [else '()])
  
  ;; Old mediKanren 1 common.rkt code:
  #;(cond
    [(assoc "pmids" eprops)  ;; WEB the 'pmids' property is only used by semmed, I believe
     => (lambda (pr) (regexp-split #rx";" (cdr pr)))]
    [(assoc "publications" eprops)
     => (lambda (pr)
          (define pubs (cdr pr))
          (if (not (string? pubs))
            '()
            (regexp-match* #rx"([0-9]+)" pubs #:match-select cadr)))]
    [else '()])
  )

(define (python->json py)
  (define len (string-length py))
  (let loop ((i 0) (start 0))
    (cond ((= i len) (if (= start 0) py (substring py start)))
          ((eqv? (string-ref py i) #\')
           (string-append
             (substring py start i) "\""
             (let requote ((i (+ i 1)) (start (+ i 1)))
               (cond ((eqv? (string-ref py i) #\')
                      (string-append (substring py start i) "\""
                                     (loop (+ i 1) (+ i 1))))
                     ((eqv? (string-ref py i) #\\)
                      (if (eqv? (string-ref py (+ i 1)) #\")
                        (requote (+ i 2) start)
                        (string-append (substring py start i)
                                       (requote (+ i 2) (+ i 1)))))
                     ((eqv? (string-ref py i) #\")
                      (string-append (substring py start i) "\\\""
                                     (requote (+ i 1) (+ i 1))))
                     (else (requote (+ i 1) start))))))
          ((eqv? (string-ref py i) #\")
           (let skip ((i (+ i 1)) (start start))
             (cond ((eqv? (string-ref py i) #\") (loop (+ i 1) start))
                   ((eqv? (string-ref py i) #\\)
                    (if (eqv? (string-ref py (+ i 1)) #\")
                      (skip (+ i 2) start)
                      (string-append (substring py start i)
                                     (skip (+ i 2) (+ i 1)))))
                   (else                         (skip (+ i 1) start)))))
          (else (loop (+ i 1) start)))))

(define PUBMED_URL_PREFIX "https://www.ncbi.nlm.nih.gov/pubmed/")
(define (pubmed-URLs-from-edge edge)
  (map (lambda (pubmed-id) (string-append PUBMED_URL_PREFIX (~a pubmed-id)))
       (pubmed-ids-from-edge edge)))

(define (pubmed-ids-from-edge edge)
  (remove-duplicates
    (match edge
      ['path-separator '()]
      [`(,dbname ,eid ,subj ,obj ,p ,eprops)
        (pubmed-ids-from-edge-props eprops)])))

(define (publications-info-alist-from-edge-props eprops)
  (cond
    [(assoc "publications_info" eprops)
     => (lambda (pr)
          (with-handlers ([exn:fail?
                           (lambda (v)
                             ((error-display-handler) (exn-message v) v)
                             '())])
            (define pubs (cdr pr))
            (define jason-ht (string->jsexpr (python->json pubs)))
            (hash-map jason-ht (lambda (k v)
                                 (cons (string-append
                                        PUBMED_URL_PREFIX
                                        (car (regexp-match* #rx"([0-9]+)" (symbol->string k) #:match-select cadr)))
                                       (list (hash-ref v '|publication date| #f)
                                             (hash-ref v '|subject score| #f)
                                             (hash-ref v '|object score| #f)
                                             (regexp-replace*
                                              #rx"([ ]+)"
                                              (hash-ref v 'sentence #f)
                                              " ")))))))]
    [else '()]))

(define (publications-info-alist-from-edge edge)
  ;; ((pubmed-URL . (publication-date subject-score object-score sentence)) ...)
  (remove-duplicates
    (match edge
      ['path-separator '()]
      [`(,dbname ,eid ,subj ,obj ,p ,eprops)
        (publications-info-alist-from-edge-props eprops)])))



(define (print-short-concept-description concepts)
  (displayln
   (map
    (lambda (c)
      (match c
        [`(,dbname ,curie ,name . ,rest)
         `(,curie ,name)]))
    concepts)))

(define (find-predicates/concepts subject? object? concepts)
  (printf "find-predicates/concepts subject?: ~s\nobject?: ~s\nconcepts: ~s\n\n" subject? object? concepts)
  (let ((ans (map
              (lambda (c)
                (match c
                  [`(,dbname ,curie ,name ,cat)

                   (define subject-predicates
                       (and subject?
                            (set->list
                             (run*/set pred
                               (fresh (eid o)
                                 (edge `(,dbname . ,eid) curie o)
                                 (eprop `(,dbname . ,eid) "predicate" pred))))))

                   (define object-predicates
                       (and object?
                            (set->list
                             (run*/set pred
                               (fresh (eid s)
                                 (edge `(,dbname . ,eid) s curie)
                                 (eprop `(,dbname . ,eid) "predicate" pred))))))
                   
                   (printf "subject-predicates: ~s\n" subject-predicates)
                   (printf "object-predicates: ~s\n" object-predicates)
                   (list c subject-predicates object-predicates)]))
              concepts)))
    ans))

;; from medikanren/common.rkt:
;;
#;(define (find-predicates/concepts subject? object? concepts)
  (map (lambda (c)
         (define subject-predicates
           (and subject? (run* (p) (subject-predicateo c p))))
         (define object-predicates
           (and object? (run* (p) (object-predicateo c p))))
         (list c subject-predicates object-predicates))
       concepts))


;; TODO implement or remove
(define (find-concepts/options/curie-infer subject? object? strings)
  '())

;; from medikanren/common.rkt:
;;
#;(define (find-concepts/options/cui-infer subject? object? isa-count strings)
  (define yes-cui
    (map (lambda (s) (run* (c) (~cui*-concepto (list s) c))) strings))
  (define no-cui (filter-not not (map (lambda (s rs) (and (null? rs) s))
                                      strings yes-cui)))
  (define all (append* (cons (run* (c) (~name*-concepto no-cui c)) yes-cui)))
  (concepts/options subject? object? isa-count all))


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


;; from medikanren/db.rkt:
;;
#;(define (smart-string-matches? case-sensitive? chars:ignore chars:split str* hay)
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






(define (split-name-string name)
  (string-split name #px"\\s+"))

(define (empty-string? str)
  (not (not (regexp-match #px"^[\\s]*$" str))))

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
          SORT_COLUMN_DECREASING
          SORT_COLUMN_DECREASING))
(define *last-concept-X-column-clicked-for-sorting* (box -1))

(define *concept-1-name-string* (box ""))
(define *concept-1-node-normalization-flag* (box #t))
(define *concept-1-lightweight-reasoning-flag* (box #f))
(define *concept-1-choices* (box '()))
(define *predicate-1-choices* (box '()))

(define *concept-2-name-string* (box ""))
(define *concept-2-node-normalization-flag* (box #t))
(define *concept-2-lightweight-reasoning-flag* (box #f))
(define *concept-2-choices* (box '()))
(define *predicate-2-choices* (box '()))

(define *concept-X-choices* (box '()))
(define *full-path-choices* (box '()))
(define *pubmed-choices* (box '()))

;; saved choices used to generate
;; paths when clicking on a concept in the X list box.
(define *solution-concept-1-name-string* (box ""))
(define *solution-concept-2-name-string* (box ""))
(define *solution-concept-1-node-normalization-flag* (box #t))
(define *solution-concept-2-node-normalization-flag* (box #t))
(define *solution-concept-1-lightweight-reasoning-flag* (box #f))
(define *solution-concept-2-lightweight-reasoning-flag* (box #f))
(define *solution-concept-1-choices* (box '()))
(define *solution-concept-2-choices* (box '()))
(define *solution-predicate-1-choices* (box '()))
(define *solution-predicate-2-choices* (box '()))

;; ((pubmed-URL . (publication-date subject-score object-score sentence)) ...)
(define *publications-info-alist* (box '()))

(define *populate-publication-fields*
  (lambda args
    (error '*populate-publication-fields* "*populate-publication-fields* function not initialized")))

(define (scheduler dependents)
  (define mk-thread #f)
  (define (kill-and-run p)
    (kill-current-thread)
    (set! mk-thread (thread p)))
  (define (kill-current-thread)
    (and mk-thread (begin (kill-thread mk-thread)
                          (set! mk-thread #f)))
    (for-each (lambda (s) (s 'kill)) dependents))
  (lambda (op . args)
    (case op
      ((run)  (apply kill-and-run args))
      ((kill) (kill-current-thread))
      (else   (error "invalid scheduler operation:" op args)))))

(define S (scheduler '()))
(define S:edges S)
(define S:X     S)
(define S:C1P   S)
(define S:C2P   S)
(define S:C1    S)
(define S:C2    S)

(define handle-search-in-Xs
  (lambda (search-in-Xs-field
           concept-X-list-box
           search-in-Xs-previous-button
           search-in-Xs-next-button
           . rest)
    (define direction (if (and (list? rest) (= (length rest) 1)) (car rest) #f))
    (define search-str (send search-in-Xs-field get-value))

    (define current-selection (send concept-X-list-box get-selection))

    (cond
      [direction

       (define count (send concept-X-list-box get-number))

       (define add1/sub1 (case direction
                           [(previous) sub1]
                           [(next) add1]
                           [else (error 'add1/sub1 "unknown direction in inc/dec")]))

       (define found-selection
         (and (> count 0)
              (let loop ((i (add1/sub1 current-selection)))
                (cond
                  [(>= i count) (loop 0)]
                  [(< i 0) (loop (- count 1))]
                  [else
                   (define data (send concept-X-list-box get-data i))
                   (define name-str (list-ref data 2))
                   (define matches?
                     (smart-string-matches? #f
                                            chars:ignore-typical
                                            ""
                                            (string-split search-str " ")
                                            name-str))
                   (cond
                     [matches? i]
                     [(= i current-selection)
                      ;; wrapped around without a match
                      #f]
                     [else (loop (add1/sub1 i))])]))))

       (if found-selection
           (when (not (equal? found-selection current-selection))
             (when current-selection
               (send concept-X-list-box select current-selection #f))
             (send concept-X-list-box select found-selection #t)
             (send concept-X-list-box set-first-visible-item found-selection))
           (begin
             (when current-selection
               (send concept-X-list-box select current-selection #f))))]
      [(empty-string? search-str)
       (when current-selection
         (send concept-X-list-box select current-selection #f))
       (send search-in-Xs-previous-button enable #f)
       (send search-in-Xs-next-button enable #f)]
      [else
       (define count (send concept-X-list-box get-number))
       (define found-selection
         (and (> count 0)
              (let loop ((i 0))
                (cond
                  [(>= i count) #f]
                  [else
                   (define data (send concept-X-list-box get-data i))
                   (define name-str (list-ref data 2))
                   (define matches?
                     (smart-string-matches? #f
                                            chars:ignore-typical
                                            ""
                                            (string-split search-str " ")
                                            name-str))
                   (if matches?
                       i
                       (loop (add1 i)))]))))

       (if found-selection
           (begin
             (send search-in-Xs-previous-button enable #t)
             (send search-in-Xs-next-button enable #t))
           (begin
             (send search-in-Xs-previous-button enable #f)
             (send search-in-Xs-next-button enable #f)))

       (if found-selection
           (when (not (equal? found-selection current-selection))
             (when current-selection
               (send concept-X-list-box select current-selection #f))
             (send concept-X-list-box select found-selection #t)
             (send concept-X-list-box set-first-visible-item found-selection))
           (begin
             (when current-selection
               (send concept-X-list-box select current-selection #f))))])))

(define (convert-concept-1/2-to-list-box-format concept)
  (match concept
    [`(,dbname ,curie ,name ,cat)
     (list (format "~a" dbname)
           (~a curie #:max-width MAX-CHAR-WIDTH #:limit-marker "...")
           (format "~a" cat)
           (~a name #:max-width MAX-CHAR-WIDTH #:limit-marker "..."))]))

(define (convert-X-concept-to-list-box-format concept)
  (match concept
    [`(,dbname ,curie ,name ,cat ,props ,max-pubmed-count ,min-pubmed-count ,pred-names ,path-length ,confidence)
     (list (format "~a" dbname)
           (~a curie #:max-width MAX-CHAR-WIDTH #:limit-marker "...")
           (~a cat #:max-width MAX-CHAR-WIDTH #:limit-marker "...")
           (~a name #:max-width MAX-CHAR-WIDTH #:limit-marker "...")
           (format "~a" max-pubmed-count)
           (format "~a" min-pubmed-count)
           (string-join pred-names ", ")
           (format "~a" path-length)
           (format "~a" confidence))]))

(define (convert-concept-1/2-to-column-sorting-format concept)
  (match concept
    [`(,dbname ,curie ,name ,cat)
     (list (format "~a" dbname)
           (~a curie #:max-width MAX-CHAR-WIDTH #:limit-marker "...")
           (~a cat #:max-width MAX-CHAR-WIDTH #:limit-marker "...")
           (~a name #:max-width MAX-CHAR-WIDTH #:limit-marker "..."))]))

(define (convert-X-concept-to-column-sorting-format concept)
  (match concept
    [`(,dbname ,curie ,name ,cat ,props ,max-pubmed-count ,min-pubmed-count ,pred-names ,path-length ,confidence)
     (list (format "~a" dbname)
           (~a curie #:max-width MAX-CHAR-WIDTH #:limit-marker "...")
           (~a cat #:max-width MAX-CHAR-WIDTH #:limit-marker "...")
           (~a name #:max-width MAX-CHAR-WIDTH #:limit-marker "...")
           max-pubmed-count
           min-pubmed-count
           (string-join pred-names ", ")
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
          (map (lambda (e) (list-ref e 3)) formatted-concepts))))

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
                                            list-box
                                            last-column-clicked-for-sorting-box
                                            column-sort-order-vector
                                            choices-box
                                            convert-values-to-column-sorting-format
                                            send-values-to-list-box)

  (printf "handle-sort-by-column-header-click called\n")

  ;; get previously selected choice's data, if any
  (define current-selection (send list-box get-selection))
  (printf "current-selection: ~s\n" current-selection)
  (define current-selection-data (and current-selection
                                      (send list-box get-data current-selection)))
  (printf "current-selection-data: ~s\n" current-selection-data)

  (when current-selection
    (send list-box select current-selection #f))

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

  ;; add choice data to each list-box entry
  (define len (length sorted-choices))
  (let loop ((i 0)
             (c* sorted-choices))
    (cond
      [(= len i) (void)]
      [else
       (send list-box set-data i (car c*))
       (loop (add1 i)
             (cdr c*))]))

  ;; select previously selected choice in its new location, if any
  (when (and current-selection current-selection-data)
    (define count (send list-box get-number))
    (printf "count: ~s\n" count)
    (define new-selection
      (let loop ((i 0))
        (cond
          [(>= i count) #f]
          [else
           (let ((d (send list-box get-data i)))
             (printf "--------\n")
             (printf "d: ~s\n" d)
             (printf "(equal? d current-selection-data): ~s\n" (equal? d current-selection-data))
             (if (equal? d current-selection-data)
                 i
                 (loop (add1 i))))])))
    (printf "new-selection: ~s\n" new-selection)
    (when new-selection
      (send list-box select new-selection #t)
      (send list-box set-first-visible-item new-selection)))

  (void))

(define (concept-list parent
                      parent-search/normalize/lw-panel
                      parent-list-boxes-panel
                      label
                      name-string
                      node-normalization-flag
                      lightweight-reasoning-flag
                      choices
                      predicate-list-box-thunk
                      predicate-choices
                      edge-type
                      last-column-clicked-for-sorting-box
                      column-sort-order-vector
                      choices-box
                      convert-values-to-column-sorting-format
                      send-values-to-list-box
                      S:C S:CP)
  (define name-field (new text-field%
                          (label label)
                          (parent parent-search/normalize/lw-panel)
                          (init-value "")
                          (callback (lambda (self event)
                                      (define name (send self get-value))
                                      (set-box! name-string name)
                                      (set-box! predicate-choices '())
                                      (send (predicate-list-box-thunk) set '())
                                      (handle)))))
  (define node-normalization-field (new check-box%
                                        (parent parent-search/normalize/lw-panel)
                                        (label "Show concept synonyms for CURIE searches")
                                        (value #t)
                                        (callback (lambda (self event) (handle)))))
  (define lightweight-reasoning-field (new check-box%
                                          (parent parent-search/normalize/lw-panel)
                                          (label "Use lightweight reasoning")
                                          (value #f)
                                          ;; TODO remove the 'deleted style to show the checkbox
                                          (style '(deleted))
                                          (callback (lambda (self event) (handle)))))
  (define concept-listbox (new smart-column-width-list-box%
                               (label label)
                               (choices '())
                               (columns '("KG" "CURIE" "Category" "Name"))
                               (parent parent-list-boxes-panel)
                               (style '(column-headers clickable-headers reorderable-headers extended))
                               (callback (lambda (self event)
                                           (define event-type (send event get-event-type))
                                           (cond
                                             [(eqv? event-type 'list-box-column)
                                              (handle-sort-by-column-header-click
                                                event
                                                concept-listbox
                                                last-column-clicked-for-sorting-box
                                                column-sort-order-vector
                                                choices-box
                                                convert-values-to-column-sorting-format
                                                send-values-to-list-box)]
                                             [else
                                               (define selections (send self get-selections))
                                               (define selected-concepts
                                                 (foldr (lambda (i l) (cons (list-ref (unbox choices) i) l)) '() selections))
                                               (when *verbose*
                                                 (printf "selected concepts:\n")
                                                 (print-short-concept-description selected-concepts)
                                                 ;; Old mediKanren 1 GUI code:
                                                 ;; (printf "selected concepts:\n~s\n" selected-concepts)
                                                 )
                                               (S:CP 'run
                                                     (thunk
                                                       (define preds-by-concept
                                                         (time (case edge-type
                                                                 [(in-edge)  (map caddr (find-predicates/concepts #f #t selected-concepts))]
                                                                 [(out-edge) (map cadr (find-predicates/concepts #t #f selected-concepts))]
                                                                 [else       (error 'concept-listbox/predicates)])))
                                                       (define predicates
                                                         (begin
                                                           (printf "preds-by-concept:\n~s\n" preds-by-concept)
                                                           (sort (remove-duplicates (apply append preds-by-concept)) string<?)
                                                           ;; Old mediKanren 1 GUI code:
                                                           ;; (sort (remove-duplicates (map cddr (append* preds-by-concept))) string<?)
                                                           ))
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
                                                            (send (predicate-list-box-thunk) select i #f))))])))))

  (define (mk-run)
    (let* ((subject? (case edge-type
                       [(out-edge) #t]
                       [(in-edge)  #f]))
           (object? (case edge-type
                      [(out-edge) #f]
                      [(in-edge)  #t]))
           (string-parts (split-name-string current-name))
           (ans (cond
                  ((null? string-parts) '())
                  ((curie-string current-name) =>
                   (lambda (cs)
                     (printf "treating '~s' as a single CURIE\n" cs)
                     (printf "performing CURIE search for: ~s\n" cs)
                     (let ((synonyms (if (unbox node-normalization-flag)
                                         (set->list
                                          (set-union
                                           (set cs)
                                           (list->set
                                            (run* x (kgx-synonym cs x)))))
                                         (list cs))))
                       (printf "found synonyms:\n~s\n" synonyms)
                       (time (set->list
                              (apply set-union
                                     (map (lambda (curie)
                                            (run*/set ans
                                              (fresh (dbname eid s o name cat)
                                                (== `(,dbname ,curie ,name ,cat) ans)
                                                (if subject?
                                                    (== curie s)
                                                    (== curie o))
                                                (edge `(,dbname . ,eid) s o)
                                                (cprop curie "name" name)
                                                (cprop curie "category" cat))))
                                          synonyms)))))))
                  (else

                   (printf "treating '~s' as a non-CURIE search\n" string-parts)
                   (printf "performing search for: ~s\n" string-parts)

                   (let ((string-search-curies (find-ids-named rtx:cprop string-parts)))
                     (time (set->list
                            (apply set-union
                                   (map (lambda (curie)
                                          (run*/set ans
                                            (fresh (dbname eid s o name cat)
                                              (== `(,dbname ,curie ,name ,cat) ans)
                                              (if subject?
                                                  (== curie s)
                                                  (== curie o))
                                              (edge `(,dbname . ,eid) s o)
                                              (cprop curie "name" name)
                                              (cprop curie "category" cat))))
                                        string-search-curies)))))                   

                   ;; Old mediKanren 1 GUI code:
                   ;; (time (find-concepts/options/cui-infer subject? object? isa-count string-parts))
                   
                   ))))

      ;; (printf "ans:\n~s\n" ans)
      
      (set-box! choices ans)
      (send concept-listbox
            set
            (map (lambda (x)
                   (match x
                     [`(,dbname ,curie ,name ,cat)
                      (~a dbname #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                 ans)          
            (map (lambda (x)
                   (match x
                     [`(,dbname ,curie ,name ,cat)
                      (~a curie #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                 ans)
            (map (lambda (x)
                   (match x
                     [`(,dbname ,curie ,name ,cat)
                      (~a cat #:max-width MAX-CHAR-WIDTH #:limit-marker "...")
                      ;; Old mediKanren 1 GUI code:
                      ;; (~a `(,catid . ,cat) #:max-width MAX-CHAR-WIDTH #:limit-marker "...")
                      ]))
                 ans)
            (map (lambda (x)
                   (match x
                     [`(,dbname ,curie ,name ,cat)
                      (~a name #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                 ans))

      ;; add choice data to each list-box entry
      (define len (length (unbox choices)))
      (let loop ((i 0)
                 (c* (unbox choices)))
        (cond
          [(= len i) (void)]
          [else
           (send concept-listbox set-data i (car c*))
           (loop (add1 i)
                 (cdr c*))]))

      ;; unselect all items
      (for ([i (length ans)])
           (send concept-listbox select i #f))))

  (define current-name "")
  (define current-node-normalization #t)
  (define current-lightweight-reasoning #f)
  (define pending-name current-name)
  (define timer (new timer% (notify-callback (thunk (S:C 'run mk-run)))))
  (define (handle)
    (define new-name (send name-field get-value))
    (define new-node-normalization (send node-normalization-field get-value))
    (define new-lightweight-reasoning (send lightweight-reasoning-field get-value))
    (when (not (and (equal? current-name new-name)
                    (equal? current-node-normalization new-node-normalization)
                    (equal? current-lightweight-reasoning new-lightweight-reasoning)))
      (set! current-name new-name)
      
      (set! current-node-normalization new-node-normalization)
      (set-box! node-normalization-flag current-node-normalization)

      (set! current-lightweight-reasoning new-lightweight-reasoning)
      (set-box! lightweight-reasoning-flag current-lightweight-reasoning)
      
      (S:C 'kill)
      (send timer stop)
      (send timer start input-response-latency #t)))
  concept-listbox)

(define (launch-gui)
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

        (printf "go callback called...\n")
        
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
        (print-short-concept-description concept-1-selected-concepts)
        ;; Old mediKanren 1 GUI code:
        ;; (displayln concept-1-selected-concepts)
        (printf "---------------------------------\n")
        (printf "concept-2-selections: ~s\n" concept-2-selections)
        (print-short-concept-description concept-2-selected-concepts)
        ;; Old mediKanren 1 GUI code:
        ;; (displayln concept-2-selected-concepts)
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

        (S:X 'run (thunk (find-X-concepts concept-1-selected-concepts
                                          concept-2-selected-concepts
                                          predicate-1-selected-predicates
                                          predicate-2-selected-predicates
                                          (unbox *predicate-1-choices*)
                                          (unbox *predicate-2-choices*)
                                          concept-X-list-box
                                          running-status-description
                                          full-path-list-box
                                          subject-properties-list-box
                                          edge-properties-list-box
                                          object-properties-list-box
                                          pubmed-list-box
                                          search-in-Xs-field
                                          search-in-Xs-previous-button
                                          search-in-Xs-next-button)))))

    (define concept-1-overall-pane (new vertical-pane%
                                        (parent upper-pane)
                                        (alignment '(left center))))

    (define concept-1-search/normalize/lw-panel (new panel:horizontal-dragable%
                                            (parent concept-1-overall-pane)
                                            (alignment '(left center))
                                            (stretchable-height #f)))
    (define concept-1-list-boxes-panel (new panel:horizontal-dragable%
                                            (parent concept-1-overall-pane)
                                            (alignment '(left center))))
    (define concept-1-list-box (concept-list concept-1-overall-pane
                                             concept-1-search/normalize/lw-panel
                                             concept-1-list-boxes-panel
                                             "Concept 1"
                                             *concept-1-name-string*
                                             *concept-1-node-normalization-flag*
                                             *concept-1-lightweight-reasoning-flag*
                                             *concept-1-choices*
                                             (lambda () predicate-1-list-box)
                                             *predicate-1-choices*
                                             'out-edge
                                             *last-concept-1-column-clicked-for-sorting*
                                             *concept-1-column-sort-order*
                                             *concept-1-choices*
                                             convert-concept-1/2-to-column-sorting-format
                                             (make-send-concepts-to-concept-1/2-list-box (lambda () concept-1-list-box))
                                             S:C1 S:C1P))
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

    (define concept-2-search/normalize/lw-panel (new panel:horizontal-dragable%
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
                                             concept-2-search/normalize/lw-panel
                                             concept-2-list-boxes-panel
                                             "Concept 2"
                                             *concept-2-name-string*
                                             *concept-2-node-normalization-flag*
                                             *concept-2-lightweight-reasoning-flag*
                                             *concept-2-choices*
                                             (lambda () predicate-2-list-box)
                                             *predicate-2-choices*
                                             'in-edge
                                             *last-concept-2-column-clicked-for-sorting*
                                             *concept-2-column-sort-order*
                                             *concept-2-choices*
                                             convert-concept-1/2-to-column-sorting-format
                                             (make-send-concepts-to-concept-1/2-list-box (lambda () concept-2-list-box))
                                             S:C2 S:C2P))

    (define running-status-description/search-in-Xs-panel
      (new
       horizontal-panel%
       (parent concept-2-overall-pane)
       (alignment '(left center))
       (stretchable-height #f)))

    (define running-status-description (new message%
                                            (parent running-status-description/search-in-Xs-panel)
                                            (label "                                                                ")))

    (define search-in-Xs-field (new text-field%
                                    (label "Find in X's")
                                    (parent running-status-description/search-in-Xs-panel)
                                    (init-value "")
                                    (callback (lambda (self event)
                                                (handle-search-in-Xs self
                                                                     concept-X-list-box
                                                                     search-in-Xs-previous-button
                                                                     search-in-Xs-next-button
                                                                     )))))

    (define search-in-Xs-previous-button (new button%
                                              (parent running-status-description/search-in-Xs-panel)
                                              (label "Previous")
                                              (callback (lambda (self event)
                                                          (handle-search-in-Xs search-in-Xs-field
                                                                               concept-X-list-box
                                                                               search-in-Xs-previous-button
                                                                               search-in-Xs-next-button
                                                                               'previous)))))

    (define search-in-Xs-next-button (new button%
                                          (parent running-status-description/search-in-Xs-panel)
                                          (label "Next")
                                          (callback (lambda (self event)
                                                      (handle-search-in-Xs search-in-Xs-field
                                                                           concept-X-list-box
                                                                           search-in-Xs-previous-button
                                                                           search-in-Xs-next-button
                                                                           'next)))))

    (define concept-X-list-box (new smart-column-width-list-box%
                                    (label "X")
                                    (choices (unbox *concept-X-choices*))
                                    (columns '("KG" "CURIE" "Category" "Name" "Max PubMed #" "Min PubMed #" "Predicates" "Path Length" "Path Confidence"))
                                    (parent lower-pane)
                                    (style '(column-headers clickable-headers reorderable-headers single))
                                    (callback (lambda (self event)
                                                (define event-type (send event get-event-type))
                                                (S:edges 'run
                                                         (thunk
                                                           (cond
                                                             [(eqv? event-type 'list-box-column)
                                                              (handle-sort-by-column-header-click
                                                                event
                                                                concept-X-list-box
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
                                                                        [`(,dbname ,curie ,name ,cat ,props ,max-pubmed-count ,min-pubmed-count ,pred-names ,path-count ,confidence)
                                                                          name]
                                                                        [else ""]))
                                                                    "")))
                                                              (printf "concept name: ~s\n" concept-name)
                                                              (send the-clipboard set-clipboard-string concept-name time-stamp)]
                                                             [else

                                                               ;; empty the entries in the full-path-list-box
                                                               (send full-path-list-box set '() '() '() '() '() '() '() '())

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
                                                                               [`(,dbname ,curie ,name ,cat ,props ,max-pubmed-count ,min-pubmed-count ,pred-names ,path-count ,confidence)
                                                                                 `(,dbname ,curie ,name ,cat ,props)])))
                                                                       (printf "selected ~s\n" selected-X)
                                                                       (define concept-1* (unbox *solution-concept-1-choices*))
                                                                       (define concept-2* (unbox *solution-concept-2-choices*))
                                                                       (printf "concept-1* ~s\n" concept-1*)
                                                                       (printf "concept-2* ~s\n" concept-2*)
                                                                       (define predicate-1* (unbox *solution-predicate-1-choices*))
                                                                       (define predicate-2* (unbox *solution-predicate-2-choices*))
                                                                       (printf "predicate-1* ~s\n" predicate-1*)
                                                                       (printf "predicate-2* ~s\n" predicate-2*)

                                                                       (define atomic/synthetic-predicate-1* (split-atomic/synthetic-predicates (unbox *predicate-1-choices*) predicate-1*))
                                                                       (define atomic/synthetic-predicate-2* (split-atomic/synthetic-predicates (unbox *predicate-2-choices*) predicate-2*))

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
                                                                          (printf "== synthetic queries 1\n")
                                                                          (set! paths '())
                                                                          ;; run synthetic queries here

                                                                          (set! paths
                                                                            (remove-duplicates
                                                                             (append paths
                                                                               (let ((ans
                                                                                      (set->list
                                                                                       (run*/set (q)
                                                                                         (fresh (dbname eid
                                                                                                        o-curie o-name o-cat
                                                                                                        x-curie x-name x-cat fake-x-props
                                                                                                        pred edge-props)
                                                                                           (== (list `(,dbname ,eid (,x-curie ,x-name ,x-cat) (,o-curie ,o-name ,o-cat) ,pred ,edge-props)) q)
                                                                                           (== `(,dbname ,x-curie ,x-name ,x-cat ,fake-x-props) selected-X)
                                                                                           
                                                                                           (membero `(,dbname ,o-curie ,o-name ,o-cat) concept-2*)
                                                                                           (membero pred atomic-predicate-2*)
                                                                                           (edge `(,dbname . ,eid) x-curie o-curie)
                                                                                           (eprop `(,dbname . ,eid) "predicate" pred)
                                                                                           (:== edge-props (dbname eid)
                                                                                                (run* k/v
                                                                                                  (fresh (k v)
                                                                                                    (== k/v `(,k . ,v))
                                                                                                    (eprop `(,dbname . ,eid) k v))))
                                                                                           )))))
                                                                                 (if (null? ans)
                                                                                     '()
                                                                                     (car ans))))))
                                                                          
                                                                          #|
                                                                          ;; Old mediKanren 1 GUI code:
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
                                                                                          (edgeo e))))))
                                                                          |#

                                                                          ]
                                                                         [(null? (split-name-string (unbox *solution-concept-2-name-string*)))
                                                                          (printf "== synthetic queries 2\n")
                                                                          (printf "concept-1*:\n~s\n" concept-1*)
                                                                          (printf "atomic-predicate-1*:\n~s\n" atomic-predicate-1*)
                                                                          (printf "selected-X:\n~s\n" selected-X)
                                                                          (set! paths '())
                                                                          ;; run synthetic queries here

                                                                          (set! paths
                                                                            (remove-duplicates
                                                                             (append paths
                                                                               (let ((ans
                                                                                      (set->list
                                                                                       (run*/set (q)
                                                                                         (fresh (dbname eid
                                                                                                        s-curie s-name s-cat
                                                                                                        x-curie x-name x-cat fake-x-props
                                                                                                        pred edge-props)
                                                                                           (== (list `(,dbname ,eid (,s-curie ,s-name ,s-cat) (,x-curie ,x-name ,x-cat) ,pred ,edge-props)) q)
                                                                                           (== `(,dbname ,x-curie ,x-name ,x-cat ,fake-x-props) selected-X)
                                                                                           
                                                                                           (membero `(,dbname ,s-curie ,s-name ,s-cat) concept-1*)
                                                                                           (membero pred atomic-predicate-1*)
                                                                                           (edge `(,dbname . ,eid) s-curie x-curie)
                                                                                           (eprop `(,dbname . ,eid) "predicate" pred)
                                                                                           (:== edge-props (dbname eid)
                                                                                                (run* k/v
                                                                                                  (fresh (k v)
                                                                                                    (== k/v `(,k . ,v))
                                                                                                    (eprop `(,dbname . ,eid) k v))))
                                                                                           )))))
                                                                                 (if (null? ans)
                                                                                     '()
                                                                                     (car ans))))))
                                                                          
                                                                          #|
                                                                          ;; Old mediKanren 1 GUI code:
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
                                                                                          (edgeo e))))))
                                                                          |#

                                                                          ]
                                                                         [else
                                                                          (printf "== synthetic queries 3\n")
                                                                           (set! paths '())
                                                                           ;; run synthetic queries here

                                                                           (printf "selected-X:\n~s\n" selected-X)
                                                                           (printf "concept-1*:\n~s\n" concept-1*)
                                                                           (printf "concept-2*:\n~s\n" concept-2*)
                                                                           (printf "atomic-predicate-1*:\n~s\n" atomic-predicate-1*)
                                                                           (printf "atomic-predicate-2*:\n~s\n" atomic-predicate-2*)
                                                                           
                                                                           (set! paths
                                                                             (remove-duplicates
                                                                              (append paths
                                                                                (let ((ans
                                                                                       (set->list
                                                                                        (run*/set (q)
                                                                                          (fresh (dbname1 dbname2
                                                                                                  eid1 eid2
                                                                                                  s-curie s-name s-cat
                                                                                                  x-curie x-name x-cat fake-x-props
                                                                                                  o-curie o-name o-cat
                                                                                                  pred1 pred2 edge1-props edge2-props)
                                                                                            (== (list
                                                                                                 `(,dbname1 ,eid1 (,s-curie ,s-name ,s-cat) (,x-curie ,x-name ,x-cat) ,pred1 ,edge1-props)
                                                                                                 `(,dbname2 ,eid2 (,x-curie ,x-name ,x-cat) (,o-curie ,o-name ,o-cat) ,pred2 ,edge2-props))
                                                                                                q)
                                                                                            (== `((,dbname1 ,dbname2) ,x-curie ,x-name ,x-cat ,fake-x-props) selected-X)
                                                                                           
                                                                                            (membero `(,dbname1 ,s-curie ,s-name ,s-cat) concept-1*)
                                                                                            (membero `(,dbname2 ,o-curie ,o-name ,o-cat) concept-2*)
                                                                                            (membero pred1 atomic-predicate-1*)
                                                                                            (membero pred2 atomic-predicate-2*)
                                                                                            (edge `(,dbname1 . ,eid1) s-curie x-curie)
                                                                                            (edge `(,dbname2 . ,eid2) x-curie o-curie)
                                                                                            (eprop `(,dbname1 . ,eid1) "predicate" pred1)
                                                                                            (eprop `(,dbname2 . ,eid2) "predicate" pred2)
                                                                                            (:== edge1-props (dbname1 eid1)
                                                                                                 (run* k/v
                                                                                                   (fresh (k v)
                                                                                                     (== k/v `(,k . ,v))
                                                                                                     (eprop `(,dbname1 . ,eid1) k v))))
                                                                                            (:== edge2-props (dbname2 eid2)
                                                                                                 (run* k/v
                                                                                                   (fresh (k v)
                                                                                                     (== k/v `(,k . ,v))
                                                                                                     (eprop `(,dbname2 . ,eid2) k v))))
                                                                                            )))))
                                                                                  (printf "ans:\n~s\n" ans)
                                                                                  (if (null? ans)
                                                                                      '()
                                                                                      (car ans))))))
                                                                           
                                                                          #|
                                                                          ;; Old mediKanren 1 GUI code:
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
                                                                                           (edgeo e2))))))
                                                                           |#

                                                                           ])

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
                                                                                (printf "x:\n~s\n" x)
                                                                                (match x
                                                                                  ['path-separator "----"]
                                                                                  [`(,dbname ,eid ,subj ,obj ,pred ,eprops)
                                                                                    (~a dbname)]))
                                                                              flattened-paths))

                                                                       (define full-path-eid-list
                                                                         (map (lambda (x)
                                                                                (match x
                                                                                  ['path-separator "----"]
                                                                                  [`(,dbname ,eid ,subj ,obj ,pred ,eprops)
                                                                                    (~a eid)]))
                                                                              flattened-paths))

                                                                       (define full-path-subj-list
                                                                         (map (lambda (x)
                                                                                (match x
                                                                                  ['path-separator "----"]
                                                                                  [`(,dbname ,eid (,curie ,name ,cat) ,obj ,pred ,eprops)
                                                                                    (~a (format "~a (~a)" name curie) #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                                                                              flattened-paths))

                                                                       (define full-path-pred-list
                                                                         (map (lambda (x)
                                                                                (match x
                                                                                  ['path-separator "----"]
                                                                                  [`(,dbname ,eid ,subj ,obj ,pred ,eprops)
                                                                                    (~a pred #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                                                                              flattened-paths))

                                                                       (define full-path-obj-list
                                                                         (map (lambda (x)
                                                                                (match x
                                                                                  ['path-separator "----"]
                                                                                  [`(,dbname ,eid ,subj (,curie ,name ,cat) ,pred ,eprops)
                                                                                    (~a (format "~a (~a)" name curie) #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                                                                              flattened-paths))

                                                                       (define full-path-subj-cat-list
                                                                         (map (lambda (x)
                                                                                (match x
                                                                                  ['path-separator "----"]
                                                                                  [`(,dbname ,eid (,curie ,name ,cat) ,obj ,pred ,eprops)
                                                                                    (~a cat #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                                                                              flattened-paths))

                                                                       (define full-path-obj-cat-list
                                                                         (map (lambda (x)
                                                                                (match x
                                                                                  ['path-separator "----"]
                                                                                  [`(,dbname ,eid ,subj (,curie ,name ,cat) ,pred ,eprops)
                                                                                    (~a cat #:max-width MAX-CHAR-WIDTH #:limit-marker "...")]))
                                                                              flattened-paths))

                                                                       (define full-path-PubMed-count-list
                                                                         (map (lambda (x)
                                                                                (match x
                                                                                  ['path-separator "----"]
                                                                                  [`(,dbname ,eid ,subj ,obj ,pred ,eprops)
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
                                                                       (populate-selected-paths))))
                                                                 (void))])))))))

    (define (populate-selected-paths)
      (when *verbose*
        (printf "(unbox *full-path-choices*):\n~s\n" (unbox *full-path-choices*)))
      (define selections (send full-path-list-box get-selections))
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
                       (,scurie ,sname ,scat)
                       (,ocurie ,oname ,ocat)
                       ,pred
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
              (define sprops (apply append (set->list (run*/set (q) (fresh (k v) (== `(,k . ,v) q) (cprop scurie k v))))))
              (define oprops (apply append (set->list (run*/set (q) (fresh (k v) (== `(,k . ,v) q) (cprop ocurie k v))))))
              (printf "eprops:\n~s\n" eprops)
              (printf "sprops:\n~s\n" sprops)
              (printf "oprops:\n~s\n" oprops)
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
      (for-each
        (lambda (edge)
          (let ((publications-info-alist
                  (publications-info-alist-from-edge edge)))
            (set-box! *publications-info-alist* publications-info-alist)))
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
          selected-full-paths)))

    (define full-path-list-box (new smart-column-width-list-box%
                                    (label "Paths")
                                    (choices (unbox *full-path-choices*))
                                    (columns '("KG" "EID" "Subject" "Predicate" "Object" "Subj Cat" "Obj Cat" "PubMed #"))
                                    (parent lower-pane)
                                    (style '(column-headers reorderable-headers extended))
                                    (callback (lambda (self event) (populate-selected-paths)))))

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

                                                 (match (assoc url (unbox *publications-info-alist*))
                                                   [`(,pubmed-URL . (,publication-date ,subject-score ,object-score ,sentence))
                                                    (*populate-publication-fields* publication-date subject-score object-score sentence)]
                                                   [#f (*populate-publication-fields* "" "" "" "")])

                                                 (when (eqv? event-type 'list-box-dclick)
                                                   ;; if the user double-clicked on the URL, open it in a web browser
                                                   (send-url url)))
                                               selected-pubmeds)))))

    (define publication-info-panel (new panel:vertical-dragable%
                                        (parent lower-pane)
                                        (alignment '(left center))
                                        (stretchable-height #t)))

    (define publication-info-date/subject/object-panel (new horizontal-panel%
                                                            (parent publication-info-panel)
                                                            (alignment '(left center))
                                                            (stretchable-height #f)))

    (define publication-date (new text-field%
                                  (label "Publication Date")
                                  (parent publication-info-date/subject/object-panel)
                                  (enabled #f)
                                  (init-value "")))

    (define subject-score (new text-field%
                               (label "Subject Score")
                               (parent publication-info-date/subject/object-panel)
                               (enabled #f)
                               (init-value "")))

    (define object-score (new text-field%
                              (label "Object Score")
                              (parent publication-info-date/subject/object-panel)
                              (enabled #f)
                              (init-value "")))

    (define publication-sentence-editor-canvas (new editor-canvas%
                                                    (parent publication-info-panel)
                                                    (enabled #f)
                                                    (label "Sentence")))
    (define publication-sentence-text (new text%))
    (send publication-sentence-text insert "")
    (send publication-sentence-editor-canvas set-editor publication-sentence-text)

    (set! *populate-publication-fields*
          (lambda (date subj-score obj-score sentence)
            (send publication-date set-value date)
            (send subject-score set-value (format "~a" subj-score))
            (send object-score set-value (format "~a" obj-score))
            (send publication-sentence-text erase)
            (send publication-sentence-text insert sentence)))


    ;; trigger reflowing of object sizes
    (send frame reflow-container)

    ;; disable previous and next buttons by default
    (send search-in-Xs-previous-button enable #f)
    (send search-in-Xs-next-button enable #f)

    (set-default-column-widths concept-1-list-box)
    (set-default-column-widths concept-2-list-box)
    (set-default-column-widths concept-X-list-box)
    (set-default-column-widths full-path-list-box)
    (set-default-column-widths edge-properties-list-box)
    (set-default-column-widths edge-properties-list-box)
    (set-default-column-widths object-properties-list-box)

    (send frame show #t)))

(define (split-atomic/synthetic-predicates choices predicate*)
  (define (synthetic? pred)
    (memf (lambda (syn-prefix) (string-prefix? pred syn-prefix))
          SYNTHETIC_PREDICATE_PREFIXES))

  (define atomic-predicate* (filter-not synthetic? predicate*))
  (define synthetic-predicate* (filter synthetic? predicate*))

  (when (memf (lambda (pred) (string-prefix? pred DECREASES_PREDICATE_PREFIX_STRING))
              synthetic-predicate*)
    (set! atomic-predicate* (append DECREASES_PREDICATE_NAMES atomic-predicate*)))

  (when (memf (lambda (pred) (string-prefix? pred INCREASES_PREDICATE_PREFIX_STRING))
              synthetic-predicate*)
    (set! atomic-predicate* (append INCREASES_PREDICATE_NAMES atomic-predicate*)))

  (set! atomic-predicate* (set-intersect choices (remove-duplicates atomic-predicate*)))

  (printf "atomic-predicate*: ~s\n" atomic-predicate*)
  (printf "synthetic-predicate*: ~s\n" synthetic-predicate*)

  (list atomic-predicate* synthetic-predicate*))


(define (find-X-concepts concept-1* concept-2* predicate-1* predicate-2* predicate-1-choices predicate-2-choices concept-X-list-box running-status-description full-path-list-box subject-properties-list-box edge-properties-list-box object-properties-list-box pubmed-list-box search-in-Xs-field search-in-Xs-previous-button search-in-Xs-next-button)

  (define start-time (current-milliseconds))
  (printf "\nfinding concepts X for which\n[C1] -> P1 -> [X] -> P2 -> [C2]\n")
  (printf "=============================\n")

  (define atomic/synthetic-predicate-1* (split-atomic/synthetic-predicates predicate-1-choices predicate-1*))
  (define atomic/synthetic-predicate-2* (split-atomic/synthetic-predicates predicate-2-choices predicate-2*))

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

  (printf "@@ entering cond\n")
  
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

     ;; run synthetic queries here

     (set! all-X-concepts-with-edges
           (remove-duplicates
            (append* all-X-concepts-with-edges
                     (let ((ans
                            (apply append
                                   (map (lambda (pred)
                                          (apply append
                                                 (map (lambda (concept)
                                                        (match concept
                                                          [`(,dbname ,o-curie ,o-name ,o-cat)
                                                           (let ((tmp
                                                                  (set->list
                                                                   (run*/set (q)
                                                                     (fresh (eid s o fake-pid edge-props e
                                                                                 o-props
                                                                                 s-curie s-name s-cat s-props)                        
                                                                       (== (list dbname s (list edge-props) (list e)) q)
                                                                       (== `(,dbname ,eid ,s ,o (,fake-pid . ,pred) ,edge-props) e)
                                                                       (== fake-pid "")
                                                                       (== `(,s-curie ,s-name ,s-cat ,s-props) s)
                                                                       (== `(,o-curie ,o-name ,o-cat ,o-props) o)
                                                                       ;; We get the concept properties later, when the user clicks on a path.
                                                                       (== s-props '())
                                                                       (== o-props '())
                                                                       ;;
                                                                       (edge `(,dbname . ,eid) s-curie o-curie)
                                                                       (eprop `(,dbname . ,eid) "predicate" pred)
                                                                       (cprop s-curie "name" s-name)
                                                                       (cprop s-curie "category" s-cat)
                                                                       (:== edge-props (dbname eid)
                                                                            (run* k/v
                                                                              (fresh (k v)
                                                                                (== k/v `(,k . ,v))
                                                                                (eprop `(,dbname . ,eid) k v)))))))))
                                                             tmp)]))
                                                      concept-2*)))
                                        atomic-predicate-2*))))
                       ;(printf "ans:\n~s\n" ans)
                       ans))))
     
     ;; Old mediKanren 1 GUI code:
     #;(set! all-X-concepts-with-edges
           (remove-duplicates
            (append all-X-concepts-with-edges
                    (run* (q)
                      (fresh (dbname eid s o pid pred eprops e)
                        (== (list dbname s (list eprops) (list e)) q)
                        (== `(,dbname ,eid ,s ,o (,pid . ,pred) ,eprops) e)
                        (membero `(,dbname . ,o) concept-2*)
                        (membero pred atomic-predicate-2*)
                        (edgeo e))))))

     ]
    [(null? (split-name-string (unbox *concept-2-name-string*)))
     (set! all-X-concepts-with-edges '())
     
     ;; run synthetic queries here
     
     (set! all-X-concepts-with-edges
           (remove-duplicates
            (append* all-X-concepts-with-edges
                    (let ((ans
                           (apply append
                                  (map (lambda (pred)
                                         (apply append
                                                (map (lambda (concept)
                                                       (match concept
                                                         [`(,dbname ,s-curie ,s-name ,s-cat)
                                                          (let ((tmp
                                                                 (set->list
                                                                  (run*/set (q)
                                                                    (fresh (eid s o fake-pid edge-props e
                                                                                s-props
                                                                                o-curie o-name o-cat o-props)                        
                                                                      (== (list dbname o (list edge-props) (list e)) q)
                                                                      (== `(,dbname ,eid ,s ,o (,fake-pid . ,pred) ,edge-props) e)
                                                                      (== fake-pid "")
                                                                      (== `(,s-curie ,s-name ,s-cat ,s-props) s)
                                                                      (== `(,o-curie ,o-name ,o-cat ,o-props) o)
                                                                      ;; We get the concept properties later, when the user clicks on a path.
                                                                      (== s-props '())
                                                                      (== o-props '())
                                                                      ;;
                                                                      (edge `(,dbname . ,eid) s-curie o-curie)
                                                                      (eprop `(,dbname . ,eid) "predicate" pred)
                                                                      (cprop o-curie "name" o-name)
                                                                      (cprop o-curie "category" o-cat)
                                                                      (:== edge-props (dbname eid)
                                                                           (run* k/v
                                                                             (fresh (k v)
                                                                               (== k/v `(,k . ,v))
                                                                               (eprop `(,dbname . ,eid) k v)))))))))
                                                            tmp)]))
                                                     concept-1*)))
                                       atomic-predicate-1*))))
                      ;(printf "ans:\n~s\n" ans)
                      ans))))
     
     ;; Old mediKanren 1 GUI code:
     #;(set! all-X-concepts-with-edges
           (remove-duplicates
            (append all-X-concepts-with-edges
                    (run* (q)
                      (fresh (dbname eid s o pid pred eprops e)
                        (== (list dbname o (list eprops) (list e)) q)
                        (== `(,dbname ,eid ,s ,o (,pid . ,pred) ,eprops) e)
                        (membero `(,dbname . ,s) concept-1*)
                        (membero pred atomic-predicate-1*)
                        (edgeo e))))))

     ]
    [else
     (set! all-X-concepts-with-edges '())

     ;; run synthetic queries here

     (set! all-X-concepts-with-edges
           (remove-duplicates
            (append* all-X-concepts-with-edges
                     (let ((ans
                            (apply append
                                   (map (lambda (pred1)
                                          (apply append
                                                 (map (lambda (pred2)
                                                        (apply append
                                                               (map (lambda (concept1)
                                                                      (match concept1
                                                                        [`(,dbname1 ,s-curie1 ,s-name1 ,s-cat1)
                                                                         (apply append
                                                                                (map (lambda (concept2)
                                                                                       (match concept2
                                                                                         [`(,dbname2 ,o-curie2 ,o-name2 ,o-cat2)

                                                                                          #|
                                                                                          (printf "<<<<<\n")
                                                                                          (printf "concept1 ~s\n" concept1)
                                                                                          (printf "concept2 ~s\n" concept2)
                                                                                          (printf "pred1 ~s\n" pred1)
                                                                                          (printf "pred2 ~s\n" pred2)
                                                                                          (printf "s-curie1 ~s\n" s-curie1)
                                                                                          (printf "o-curie2 ~s\n" o-curie2)
                                                                                          |#
                                                                   
                                                                                          (let ((tmp
                                                                                                 (set->list
                                                                                                  (run*/set (q)
                                                                                                    (fresh (eid1 s1 fake-pid1 edge-props1 e1
                                                                                                                 eid2 o2 fake-pid2 edge-props2 e2
                                                                                                                 m
                                                                                                                 m-curie m-name m-cat m-props
                                                                                                                 s-props1
                                                                                                                 o-props2)
                                                                                                      (== (list (list dbname1 dbname2) m (list edge-props1 edge-props2) (list e1 e2)) q)
                                                                                                      (== `(,dbname1 ,eid1 ,s1 ,m (,fake-pid1 . ,pred1) ,edge-props1) e1)
                                                                                                      (== `(,dbname2 ,eid2 ,m ,o2 (,fake-pid2 . ,pred2) ,edge-props2) e2)
                                                                                                      (== fake-pid1 "")
                                                                                                      (== fake-pid2 "")
                                                                                                      (== `(,m-curie ,m-name ,m-cat ,m-props) m)
                                                                                                      (== `(,s-curie1 ,s-name1 ,s-cat1 ,s-props1) s1)
                                                                                                      (== `(,o-curie2 ,o-name2 ,o-cat2 ,o-props2) o2)
                                                                                                      ;; We get these concept properties later, when the user clicks on a path.
                                                                                                      (== m-props '())
                                                                                                      (== s-props1 '())
                                                                                                      (== o-props2 '())
                                                                                                      ;;
                                                                                                      (edge `(,dbname1 . ,eid1) s-curie1 m-curie)
                                                                                                      (edge `(,dbname2 . ,eid2) m-curie o-curie2)
                                                                               
                                                                                                      (eprop `(,dbname1 . ,eid1) "predicate" pred1)
                                                                                                      (eprop `(,dbname2 . ,eid2) "predicate" pred2)
                                                                               
                                                                                                      (cprop m-curie "name" m-name)
                                                                                                      (cprop m-curie "category" m-cat)
                                                                               
                                                                                                      (:== edge-props1 (dbname1 eid1)
                                                                                                           (begin
                                                                                                             ;(printf "reached alpha\n")
                                                                                                             (run* k/v
                                                                                                               (fresh (k v)
                                                                                                                 (== k/v `(,k . ,v))
                                                                                                                 (eprop `(,dbname1 . ,eid1) k v)))))
                                                                                                      (:== edge-props2 (dbname2 eid2)
                                                                                                           (begin
                                                                                                             ;(printf "reached beta\n")
                                                                                                             (run* k/v
                                                                                                               (fresh (k v)
                                                                                                                 (== k/v `(,k . ,v))
                                                                                                                 (eprop `(,dbname2 . ,eid2) k v))))))))))
                                                                                            ;(printf "tmp:\n~s\n" tmp)
                                                                                            tmp)]))
                                                                                     concept-2*))]))
                                                                    concept-1*)))
                                                      atomic-predicate-2*)))
                                        atomic-predicate-1*))))
                       ;(printf "ans:\n~s\n" ans)
                       ans))))
     
     ;; Old mediKanren 1 GUI code:
     #;(set! all-X-concepts-with-edges
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
                        (edgeo e2))))))

     ])

  (printf "@@ exiting cond\n")

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
           (not (path-confidence<? e1* e2*))]
          [else (error 'find-X-concepts/all-X-concepts-with-edges (format "unmatched (list c1 c2) ~s\n" (list c1 c2)))]))))

  (printf "sorted all-X-concepts-with-edges\n")

  (define db/curie-to-pred-names-hash-table (make-hash))
  (let loop ([c* all-X-concepts-with-edges])
    (cond
      [(null? c*) (void)]
      [else (match (car c*)
              [`(,dbname (,curie ,name ,cat ,props) ,whatever ,e*)
               (let ((pred-names (get-pred-names e*)))
                 (let ((key (list dbname curie)))
                   (let ((current-v (hash-ref db/curie-to-pred-names-hash-table key #f)))
                     (if current-v
                         (hash-set! db/curie-to-pred-names-hash-table key (set-union pred-names current-v))
                         (hash-set! db/curie-to-pred-names-hash-table key pred-names))
                     (loop (cdr c*)))))]
              [else (error 'find-X-concepts/loop1 (format "unmatched (car c*) ~s\n" (car c*)))])]))

  (printf "filled db/curie-to-pred-names-hash-table\n")

  (define all-X-concepts '())
  (set! all-X-concepts
        (let loop ([ls all-X-concepts-with-edges])
          (cond
            [(null? ls) '()]
            [else
             (match (car ls)
               [`(,dbname (,curie ,name ,cat ,props) ,whatever ,e*)
                (let ((pubmed-count* (map pubmed-count e*))
                      (pred-names (sort (hash-ref db/curie-to-pred-names-hash-table (list dbname curie) '()) string<?)))
                  (let ((max-pubmed-count (apply max pubmed-count*))
                        (min-pubmed-count (apply min pubmed-count*))
                        (path-length (length pubmed-count*))
                        (confidence (path-confidence e*)))
                    (cons `(,dbname
                            (,curie
                             ,name
                             ,cat
                             ,props
                             ,max-pubmed-count
                             ,min-pubmed-count
                             ,pred-names
                             ,path-length
                             ,confidence)
                            ,whatever
                            ,e*)
                          (loop (remf* (lambda (x)
                                         (match x
                                           [`(,dbname-x (,curie-x ,name-x ,cat-x ,props-x) . ,rest-x)
                                            (and (equal? dbname dbname-x)
                                                 (equal? curie curie-x))]))
                                       (cdr ls))))))]
               [else (error 'find-X-concepts/loop2 (format "unmatched (car ls) ~s\n" (car ls)))])]))
        )
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

  (set-box! *solution-concept-1-node-normalization-flag* (unbox *concept-1-node-normalization-flag*))
  (set-box! *solution-concept-2-node-normalization-flag* (unbox *concept-2-node-normalization-flag*))

  (set-box! *solution-concept-1-lightweight-reasoning-flag* (unbox *concept-1-lightweight-reasoning-flag*))
  (set-box! *solution-concept-2-lightweight-reasoning-flag* (unbox *concept-2-lightweight-reasoning-flag*))
  
  (set-box! *solution-concept-1-choices* concept-1*)
  (set-box! *solution-concept-2-choices* concept-2*)
  (set-box! *solution-predicate-1-choices* predicate-1*)
  (set-box! *solution-predicate-2-choices* predicate-2*)

  (printf "*solution-concept-1-name-string*:\n~s\n" (unbox *solution-concept-1-name-string*))
  (printf "*solution-concept-1-node-normalization-flag*:\n~s\n" (unbox *solution-concept-1-node-normalization-flag*))
  (printf "*solution-concept-1-lightweight-reasoning-flag*:\n~s\n" (unbox *solution-concept-1-lightweight-reasoning-flag*))
  ;(printf "*solution-concept-1-choices*:\n")
  ;(pretty-print (unbox *solution-concept-1-choices*))
  ;(printf "*solution-predicate-1-choices*:\n")
  ;(pretty-print (unbox *solution-predicate-1-choices*))
  (newline)

  (printf "*solution-concept-2-name-string*:\n~s\n" (unbox *solution-concept-2-name-string*))
  (printf "*solution-concept-2-node-normalization-flag*:\n~s\n" (unbox *solution-concept-2-node-normalization-flag*))
  (printf "*solution-concept-2-lightweight-reasoning-flag*:\n~s\n" (unbox *solution-concept-2-lightweight-reasoning-flag*))
  ;(printf "*solution-concept-2-choices*:\n")
  ;(pretty-print (unbox *solution-concept-2-choices*))
  ;(printf "*solution-predicate-2-choices*:\n")
  ;(pretty-print (unbox *solution-predicate-2-choices*))
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
                       (,scurie ,sname ,scat . ,sprops)
                       (,ocurie ,oname ,ocat . ,oprops)
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

              "KG Name")
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
                       (,scurie ,sname ,scat . ,sprops)
                       (,ocurie ,oname ,ocat . ,oprops)
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

  ;; add X concept data for each list-box entry
  (let loop ((i 0)
             (c* all-X-concepts))
    (cond
      [(null? c*) (void)]
      [else
       (send concept-X-list-box set-data i (car c*))
       (loop (add1 i)
             (cdr c*))]))

  ;; empty the search in X's field, and disable the previous/next buttons
  (send search-in-Xs-field set-value "")
  (send search-in-Xs-previous-button enable #f)
  (send search-in-Xs-next-button enable #f)

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
