#lang racket/base
(provide
  find-concepts
  find-predicates/concepts
  find-Xs
  find-graph

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

  get-pred-names

  databases
  load-databases
  conde/databases

  config
  config-ref
  load-config

  keep
  read/file
  read/string

  path-simple
  path/data
  path:data
  path/root
  path:root)

(require
  "db.rkt"
  "mk-db.rkt"
  "repr.rkt"
  racket/format
  racket/list
  (except-in racket/match ==)
  racket/runtime-path
  racket/port
  racket/set
  racket/stream)

(define (keep n xs)
  (if (or (and n (= n 0)) (null? xs)) '()
    (cons (car xs) (keep (and n (- n 1)) (cdr xs)))))
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

(define (find-Xs subjects? objects?)
  ;; subjects?: #f | ((concept (predicate ...) #f) ...)
  ;; objects?:  #f | ((concept #f (predicate ...)) ...)
  (define (group-by-X edges)
    (foldl (lambda (edge groups)
             (define db        (car edge))
             (define X         (cons db (cadr edge)))
             (define concept   (cons db (caddr edge)))
             (define predicate (cons db (cadddr edge)))
             (define einfo     (cons db (cddddr edge)))
             (define X-groups  (hash-ref groups X (hash)))
             (define c-group   (hash-ref X-groups concept (list)))
             (define c-group^  (cons (list predicate einfo) c-group))
             (hash-set groups X (hash-set X-groups concept c-group^)))
           (hash) edges))
  (define (groups-flatten groups)
    (and groups (foldl (lambda (kv groups)
                         (define X        (car kv))
                         (define X-groups (cdr kv))
                         (cons (list X (hash->list X-groups)) groups))
                       '() (hash->list groups))))
  (define (groups-intersect gS gO)
    (foldl (lambda (kv groups)
             (define X          (car kv))
             (define o-X-groups (cdr kv))
             (define s-X-groups (hash-ref gS X #f))
             (if s-X-groups
               (cons (list X (hash->list s-X-groups) (hash->list o-X-groups))
                     groups)
               groups))
           '() (hash->list gO)))
  (define subject-edges?
    (and subjects?
         (group-by-X (run* (q)
                       (fresh (dbname eid s m predicates p eprops _)
                         (== (list dbname m s p eid eprops) q)
                         (membero `((,dbname . ,s) ,predicates ,_) subjects?)
                         (membero `(,dbname . ,p) predicates)
                         (edgeo `(,dbname ,eid ,s ,m ,p ,eprops)))))))
  (define object-edges?
    (and objects?
         (group-by-X (run* (q)
                       (fresh (dbname eid o m predicates p eprops _)
                         (== (list dbname m o p eid eprops) q)
                         (membero `((,dbname . ,o) ,_ ,predicates) objects?)
                         (membero `(,dbname . ,p) predicates)
                         (edgeo `(,dbname ,eid ,m ,o ,p ,eprops)))))))
  (if (and subject-edges? object-edges?)
    (groups-intersect subject-edges? object-edges?)
    (groups-flatten (or subject-edges? object-edges?))))

(define (find-graph concept=>set concept=>cx predicate=>cx edges)
  (define-syntax-rule (set!/combine hash-name key value default combine)
                      (let* ((current (hash-ref hash-name key default))
                             (new (if current (combine current value) value)))
                        (set! hash-name (hash-set hash-name key new))
                        (and (not (equal? current new)) new)))
  (define-syntax-rule (set!/add hash-name key value)
                      (set!/combine hash-name key value (set) set-add))
  (define-syntax-rule (set!/intersect hash-name key value)
                      (set!/combine hash-name key value #f set-intersect))
  (define edge=>set      (hash))
  (define concept=>edges (hash))
  (define (concept->set   name) (hash-ref concept=>set   name #f))
  (define (concept->cx    name) (hash-ref concept=>cx    name #f))
  (define (predicate->cx  name) (hash-ref predicate=>cx  name #f))
  (define (edge->set      name) (hash-ref edge=>set      name #f))
  (define (concept->edges name) (hash-ref concept=>edges name (set)))

  (define (?min x y) (or (and x (or (and y (min (set-count x) (set-count y)))
                                    (set-count x)))
                         (and y (set-count y))))
  (define (?< x y)   (or (and x y (< x y)) x))
  (define/match (edge<? e1 e2)
    ((`(,s1 ,p1 ,o1) `(,s2 ,p2 ,o2))
     (define min1 (?min (concept->set s1) (concept->set o1)))
     (define min2 (?min (concept->set s2) (concept->set o2)))
     (or (?< min1 min2)
         (and (not (?< min2 min1))
              (let ((min1 (?min (concept->cx s1) (concept->cx o1)))
                    (min2 (?min (concept->cx s2) (concept->cx o2))))
                (or (?< min1 min2)
                    (and (not ?< min2 min1)
                         (let ((pcx1 (predicate->cx p1))
                               (pcx2 (predicate->cx p2)))
                           (?< (and pcx1 (set-count pcx1))
                               (and pcx2 (set-count pcx2)))))))))))

  (define (update-edge! e s o id-edges)
    (define enew? (set!/intersect edge=>set e id-edges))
    (when enew?
      (define id-subjects
        (list->set
          (set-map enew?
                   (match-lambda ((list dbname iedge isubject iobject)
                                  (cons dbname isubject))))))
      (define id-objects
        (list->set
          (set-map enew?
                   (match-lambda ((list dbname iedge isubject iobject)
                                  (cons dbname iobject))))))
      (define snew? (set!/intersect concept=>set s id-subjects))
      (define onew? (set!/intersect concept=>set o id-objects))
      (define (propagate! c cnew?)
        (when cnew?
          (set-for-each
            (concept->edges c)
            (match-lambda ((list ename sname oname)
                           (constrain-edge! ename sname oname))))))
      (propagate! s snew?)
      (propagate! o onew?)))
  (define (constrain-edge! e s o)
    (define sids (concept->set s))
    (define oids (concept->set o))
    (define id-edges
      (list->set (filter (match-lambda
                           ((list dbname iedge isubject iobject)
                            (and (set-member? sids (cons dbname isubject))
                                 (set-member? oids (cons dbname iobject)))))
                         (set->list (edge->set e)))))
    (update-edge! e s o id-edges))

  (let loop ((pending edges))
    (unless (null? pending)
      (define edges (sort pending edge<?))
      (define edge (car edges))
      (match-define `(,e ,s ,o) edge)
      (set!/add concept=>edges s edge)
      (set!/add concept=>edges o edge)
      (define ss  (concept->set s))
      (define os  (concept->set o))
      ;; TODO: improve these constraints when adjacent concepts are available.
      (define pcx (predicate->cx e))
      (define (find-edges/db db stream-edges srcs pids cats dsts)
        (define srcids
          (sort
            (if srcs srcs
              (run* (src)
                (fresh (src cui cname catid catname cprops)
                  (if cats (membero catid cats) (== #t #t))
                  (db:concepto
                    db `(,src ,cui ,cname (,catid . ,catname) ,cprops)))))
            <))
        (define (src->edges src)
          (define (stream/pid pid)
            (cond (dsts (append* (map (lambda (dst)
                                        (stream->list
                                          (stream-edges src pid #f dst)))
                                      dsts)))
                  (cats (append* (map (lambda (cat)
                                        (stream->list
                                          (stream-edges src pid cat #f)))
                                      cats)))
                  (else (stream->list (stream-edges src pid #f #f)))))
          (if pids (append* (map stream/pid pids)) (stream/pid #f)))
        (map (lambda (e)
               (define eid (edge-eid e))
               (define e/p (db:eid->edge db eid))
               (list eid (edge/props-subject e/p) (edge/props-object e/p)))
             (append* (map src->edges srcids))))
      (define (find-edges stream-edges/db srcs cats dsts)
        (append*
          (map (lambda (dbdesc)
                 (define dbname (car dbdesc))
                 (define db     (cdr dbdesc))
                 (define (filter/db xs)
                   (map cdr (filter (lambda (x) (equal? dbname (car x))) xs)))
                 (map (lambda (e) (cons dbname e))
                      (find-edges/db
                        db (lambda args (apply stream-edges/db db args))
                        (and srcs (filter/db (set->list srcs)))
                        (and pcx  (filter/db (set->list pcx)))
                        (and cats (filter/db (set->list cats)))
                        (and dsts (filter/db (set->list dsts))))))
               (databases))))
      (define id-edges
        (if (and os (= (set-count os) (?min ss os)))
          (find-edges db:object->edge-stream os (concept->cx s) ss)
          (find-edges db:subject->edge-stream ss (concept->cx o) os)))
      (update-edge! e s o (list->set id-edges))
      (loop (cdr edges))))
  (cons concept=>set edge=>set))

;; TODO: run/graph
