#lang racket/base
(provide
  find-concepts
  find-predicates/concepts

  membero

  ~name*-concepto
  ~cui-concepto
  ~categoryo
  ~predicateo
  edgeo
  pmid-edgeo
  subject-predicateo
  object-predicateo
  isao

  pubmed-URLs-from-edge
  pubmed-count
  path-confidence
  path-confidence<?
  sort-paths

  databases
  load-databases
  conde/databases

  config
  config-ref
  load-config

  read/file
  read/string

  path-simple
  path/data
  path:data
  path/root
  path:root)

(require
  "mk-db.rkt"
  racket/format
  racket/list
  (except-in racket/match ==)
  racket/runtime-path
  racket/port)

(define (read/file path)  (with-input-from-file  path (lambda () (read))))
(define (read/string str) (with-input-from-string str (lambda () (read))))

(define-runtime-path path:root ".")
(define (path/root relative-path) (build-path path:root relative-path))
(define path:data                 (path/root "data"))
(define (path/data relative-path) (build-path path:data relative-path))
(define (path-simple path)        (path->string (simplify-path path)))

(define box:config (box #f))
(define (config)
  (define cfg (unbox box:config))
  (cond (cfg cfg)
        (else (load-config #t #f)
              (unbox box:config))))
(define (config-ref key)
  (define kv (assoc key (config)))
  (unless kv (error "missing configuration key:" key))
  (cdr kv))
(define (load-config verbose? path:config)
  (define path:config.user     (or path:config (path/root "config.scm")))
  (define path:config.defaults (path/root "config.defaults.scm"))
  (when verbose? (printf "loading configuration defaults: ~a\n"
                         (path-simple path:config.defaults)))
  (when verbose? (printf "loading configuration overrides: ~a\n"
                         (path-simple path:config.user)))
  (define config.user     (if (file-exists? path:config.user)
                            (read/file path:config.user)
                            '()))
  (define config.defaults (read/file path:config.defaults))
  (unless (and (list? config.user) (andmap pair? config.user))
    (error "invalid configuration overrides:" config.user))
  (define user-keys (map car config.user))
  (define (user-defined? kv) (member (car kv) user-keys))
  (set-box! box:config
            (append config.user (filter-not user-defined? config.defaults))))

(define box:databases (box #f))
(define (databases)
  (define dbs (unbox box:databases))
  (cond (dbs dbs)
        (else (load-databases #t)
              (unbox box:databases))))
(define (load-databases verbose?)
  (define (load-dbs)
    (filter (lambda (desc) desc)
            (map (lambda (name)
                   (define path (path/data (symbol->string name)))
                   (cond ((directory-exists? path)
                          (when verbose? (printf "loading ~a\n" name))
                          (cons name (if verbose?
                                       (time (make-db path))
                                       (make-db path))))
                         (else (when verbose?
                                 (printf "cannot load ~a; " name)
                                 (printf "directory missing: ~a\n" path))
                               #f)))
                 (config-ref 'databases))))
  (unless (unbox box:databases)
    (when verbose? (displayln "loading data sources..."))
    (define dbs (load-dbs))
    (set-box! box:databases dbs)
    (when verbose? (displayln "finished loading data sources"))))
(define (conde/databases dbdesc->clause)
  (foldr (lambda (desc rest)
           (conde ((dbdesc->clause (car desc) (cdr desc))) (rest)))
         (== #t #f) (databases)))

;; list membership
(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        [(== x y)]
        [(=/= x y) (membero x rest)]))))

#|
concept = `(,dbname ,cid ,cui ,name (,catid . ,cat) ,props)
|#
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
(define (~cui-concepto ~cui concept)
  (conde/databases
    (lambda (dbname db)
      (fresh (c) (== `(,dbname . ,c) concept)
        (db:~cui-concepto db ~cui c)))))
(define (~categoryo ~category-name category)
  (conde/databases
    (lambda (dbname db)
      (fresh (c)
        (== `(,dbname . ,c) category)
        (db:~categoryo db ~category-name c)))))
(define (~predicateo ~predicate-name predicate)
  (conde/databases
    (lambda (dbname db)
      (fresh (p)
        (== `(,dbname . ,p) predicate)
        (db:~predicateo db ~predicate-name p)))))

#|
edge = `(,dbname ,eid (,scid ,scui ,sname (,scatid . ,scat) ,sprops)
                      (,ocid ,ocui ,oname (,ocatid . ,ocat) ,oprops)
                      (,pid . ,pred) ,eprops)
|#
(define (edgeo edge)
  (conde/databases
    (lambda (dbname db)
      (fresh (e)
        (== `(,dbname . ,e) edge)
        (db:edgeo db e)))))
(define (pmid-edgeo pmid edge)
  (conde/databases
    (lambda (dbname db)
      (fresh (eid body)
        (== `(,dbname ,eid . ,body) edge)
        (db:pmid-eido db pmid eid)
        (edgeo edge)))))

(define (subject-predicateo concept predicate)
  (conde/databases
    (lambda (dbname db)
      (fresh (c p)
        (== `(,dbname . ,c) concept)
        (== `(,dbname . ,p) predicate)
        (db:subject-predicateo db c p)))))
(define (object-predicateo concept predicate)
  (conde/databases
    (lambda (dbname db)
      (fresh (c p)
        (== `(,dbname . ,c) concept)
        (== `(,dbname . ,p) predicate)
        (db:object-predicateo db c p)))))

(define (isao s/db o/db)
  (fresh (dbname s o eid pid eprops)
    (== `(,dbname . ,o) o/db)
    (== `(,dbname . ,s) s/db)
    (edgeo `(,dbname ,eid ,s ,o (,pid . "subclass_of") ,eprops))))

(define PUBMED_URL_PREFIX "https://www.ncbi.nlm.nih.gov/pubmed/")
(define (pubmed-URLs-from-edge edge)
  (define pubmed*
    (match edge
      ['path-separator '()]
      [`(,dbname ,eid ,subj ,obj ,p ,eprops)
       (cond
         [(assoc "pmids" eprops) => ;; WEB the 'pmids' property is only used by semmed, I believe
          (lambda (pr) (regexp-split #rx";" (cdr pr)))]
         [(assoc "publications" eprops) =>
          (lambda (pr)
            (define pubs (cdr pr))
            (if (not (string? pubs))
                '()
                (regexp-match* #rx"([0-9]+)" pubs #:match-select cadr)))]
         [else '()])]))
  (map (lambda (pubmed-id) (string-append PUBMED_URL_PREFIX (~a pubmed-id)))
       (remove-duplicates pubmed*)))

(define (pubmed-count e)
  (length (pubmed-URLs-from-edge e)))

(define (path-confidence p)
  (define (weight-linear+1 n) (+ 1 n))
  (define (weight-exponential n) (expt 2 n))
  ;; To experiment with sorting, try to only change the weight calculation
  ;; being used.  Leave everything else the same.
  (define weight weight-exponential)
  (define (confidence/edge e) (- 1 (/ 1.0 (weight (pubmed-count e)))))
  (foldl * 1 (map confidence/edge p)))
(define (path-confidence<? p1 p2)
  (let ((pc1 (path-confidence p1))
        (pc2 (path-confidence p2)))
    (cond
      [(= pc1 pc2)
       (let ((pubmed-count*1 (map pubmed-count p1))
             (pubmed-count*2 (map pubmed-count p2)))
         (let ((min-pubmed-count1 (apply min pubmed-count*1))
               (min-pubmed-count2 (apply min pubmed-count*2)))
           (cond
             [(= min-pubmed-count1 min-pubmed-count2)
              (let ((max-pubmed-count1 (apply max pubmed-count*1))
                    (max-pubmed-count2 (apply max pubmed-count*2)))
                (not (> max-pubmed-count1 max-pubmed-count2)))]
             [(< min-pubmed-count1 min-pubmed-count2) #t]
             [else #f])))]
      [(< pc1 pc2) #t]
      [else #f])))
(define (sort-paths paths) (sort paths path-confidence<?))

(define (find-concepts subject? object? isa-count via-cui? strings)
  ;; subject? and object? insist that a concept participate in a certain role.
  ;; TODO: If via-cui? then strings is an OR-list of CUIs to consider.
  ;; Otherwise, strings is an AND-list of fragments the name must contain.
  (let* ((ans (run* (c) (~name*-concepto strings c)))
         (isa-ans (remove-duplicates
                    (run isa-count (s/db)
                      (fresh (o/db)
                        (membero o/db ans)
                        (isao s/db o/db)))))
         (ans (if (null? isa-ans) ans
                (remove-duplicates (append ans isa-ans))))
         (ans (filter  ;; Only include concepts with at least one predicate.
                (lambda (concept)
                  (define (? cpo) (not (null? (run 1 (p) (cpo concept p)))))
                  (and (or (not subject?) (? subject-predicateo))
                       (or (not object?) (? object-predicateo))))
                ans)))
    (sort ans (lambda (a1 a2)
                (let ((dbname1 (symbol->string (car a1)))
                      (cui1 (caddr a1))
                      (dbname2 (symbol->string (car a2)))
                      (cui2 (caddr a2)))
                  (or (string>? dbname1 dbname2)
                      (and (string=? dbname1 dbname2)
                           (string<? cui1 cui2))))))))

(define (find-predicates/concepts subject? object? concepts)
  (map (lambda (c)
         (define subject-predicates
           (and subject? (run* (p) (subject-predicateo c p))))
         (define object-predicates
           (and object? (run* (p) (object-predicateo c p))))
         (list c subject-predicates object-predicates))
       concepts))
