#lang racket/base
(provide
  (all-from-out "mk.rkt")
  find-concepts
  find-isa-concepts
  find-concepts/options
  find-concepts/options/cui-infer
  find-predicates/concepts
  find-predicates
  find-exact-predicates
  find-categories
  find-exact-categories
  find-Xs
  find-graph
  run/graph

  membero

  concept->dbname
  concept->cid
  concept->curie
  concept->name
  concept->category
  concept->props

  edge->dbname
  edge->eid
  edge->subject
  edge->object
  edge->pred
  edge->props

  ~name*-concepto
  ~cui*-concepto
  ~cui-concepto
  ~categoryo
  ~predicateo
  xref-concepto
  category-concepto
  concepto
  categoryo
  predicateo
  edgeo
  pmid-edgeo
  subject-predicateo
  object-predicateo
  isao

  pubmed-ids-from-edge-props
  pubmed-ids-from-edge
  pubmed-URLs-from-edge
  pubmed-count

  publications-info-alist-from-edge
  publications-info-alist-from-edge-props

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
  "mk.rkt"
  "mk-db.rkt"
  "repr.rkt"
  json
  racket/format
  racket/list
  (except-in racket/match ==)
  racket/runtime-path
  racket/port
  racket/set
  racket/stream
  racket/string)

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
(define (concept->dbname concept) (car concept))
(define (concept->cid concept) (cadr concept))
(define (concept->curie concept) (caddr concept))
(define (concept->name concept) (cadddr concept))
(define (concept->category concept) (cadddr (cdr concept)))
(define (concept->props concept) (cadddr (cddr concept)))

#|
edge = `(,dbname ,eid ,subject-concept ,object-concept (,pid . ,pred-name) ,props)
|#
(define (edge->dbname edge) (car edge))
(define (edge->eid edge) (cadr edge))
(define (edge->subject edge) (caddr edge))
(define (edge->object edge) (cadddr edge))
(define (edge->pred edge) (cadddr (cdr edge)))
(define (edge->props edge) (cadddr (cddr edge)))


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
(define (~cui-concepto ~cui concept)
  (conde/databases
    (lambda (dbname db)
      (fresh (c) (== `(,dbname . ,c) concept)
        (db:~cui-concepto db ~cui c)))))
(define (xref-concepto xref concept)
  (conde/databases
    (lambda (dbname db)
      (fresh (c) (== `(,dbname . ,c) concept)
        (db:xref-concepto db xref c)))))
(define (category-concepto category concept)
  (fresh (db cid cui name cat props)
    (== category `(,db . ,cat))
    (== concept `(,db ,cid ,cui ,name ,cat . ,props))
    (concepto concept)))
(define (concepto concept)
  (conde/databases
    (lambda (dbname db)
      (fresh (c)
        (== `(,dbname . ,c) concept)
        (db:concepto db c)))))

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

(define (categoryo category)
  (conde/databases
    (lambda (dbname db)
      (fresh (c)
        (== `(,dbname . ,c) category)
        (db:categoryo db c)))))
(define (predicateo predicate)
  (conde/databases
    (lambda (dbname db)
      (fresh (p)
        (== `(,dbname . ,p) predicate)
        (db:predicateo db p)))))

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

(define (pubmed-ids-from-edge-props eprops)
  (cond
    [(assoc "pmids" eprops)  ;; WEB the 'pmids' property is only used by semmed, I believe
     => (lambda (pr) (regexp-split #rx";" (cdr pr)))]
    [(assoc "publications" eprops)
     => (lambda (pr)
          (define pubs (cdr pr))
          (if (not (string? pubs))
            '()
            (regexp-match* #rx"([0-9]+)" pubs #:match-select cadr)))]
    [else '()]))
(define (pubmed-ids-from-edge edge)
  (remove-duplicates
    (match edge
      ['path-separator '()]
      [`(,dbname ,eid ,subj ,obj ,p ,eprops)
        (pubmed-ids-from-edge-props eprops)])))
(define PUBMED_URL_PREFIX "https://www.ncbi.nlm.nih.gov/pubmed/")
(define (pubmed-URLs-from-edge edge)
  (map (lambda (pubmed-id) (string-append PUBMED_URL_PREFIX (~a pubmed-id)))
       (pubmed-ids-from-edge edge)))

(define (pubmed-count e)
  (length (pubmed-ids-from-edge e)))

(define (publications-info-alist-from-edge-props eprops)
  (cond
    [(assoc "publications_info" eprops) ;; RTX2
     => (lambda (pr)
          (define pubs (cdr pr))
          (define pubs-json-str (string-replace pubs "'" "\""))
          (define jason-ht (string->jsexpr pubs-json-str))
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
                                             " "))))))]
    [else '()]))
(define (publications-info-alist-from-edge edge)
  ;; ((pubmed-URL . (publication-date subject-score object-score sentence)) ...)
  (remove-duplicates
    (match edge
      ['path-separator '()]
      [`(,dbname ,eid ,subj ,obj ,p ,eprops)
        (publications-info-alist-from-edge-props eprops)])))

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

(define (find-isa-concepts count concepts)
  (remove-duplicates (run count (s/db)
                       (fresh (o/db)
                         (membero o/db concepts)
                         (isao s/db o/db)))))

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

(define (find-concepts/options/cui-infer subject? object? isa-count strings)
  (define yes-cui
    (map (lambda (s) (run* (c) (~cui*-concepto (list s) c))) strings))
  (define no-cui (filter-not not (map (lambda (s rs) (and (null? rs) s))
                                      strings yes-cui)))
  (define all (append* (cons (run* (c) (~name*-concepto no-cui c)) yes-cui)))
  (concepts/options subject? object? isa-count all))

(define (find-concepts/options subject? object? isa-count via-cui? strings)
  (concepts/options subject? object? isa-count
                    (if via-cui?
                      (run* (c) (~cui*-concepto strings c))
                      (run* (c) (~name*-concepto strings c)))))

(define (find-concepts via-cui? strings)
  (find-concepts/options #f #f 0 via-cui? strings))

(define (find-predicates/concepts subject? object? concepts)
  (map (lambda (c)
         (define subject-predicates
           (and subject? (run* (p) (subject-predicateo c p))))
         (define object-predicates
           (and object? (run* (p) (object-predicateo c p))))
         (list c subject-predicates object-predicates))
       concepts))
(define (find-predicates names)
  (append* (map (lambda (name) (run* (x) (~predicateo name x))) names)))
(define (find-exact-predicates names)
  (run* (p) (fresh (db pid name)
                (membero name names)
                (== p `(,db ,pid . ,name))
                (predicateo p))))
(define (find-categories names)
  (append* (map (lambda (name) (run* (x) (~categoryo name x))) names)))
(define (find-exact-categories names)
  (run* (cat) (fresh (db catid name)
                (membero name names)
                (== cat `(,db ,catid . ,name))
                (categoryo cat))))

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
                    (and (not (?< min2 min1))
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
  (define (concepts->predicates cpo cs)
    (constraints->set
      (append* (map (lambda (c) (run* (p) (cpo (report-concept c) p)))
                    (set->list cs)))))
  (define (concept-predicate-intersect cpo pcx cs)
    (if (and pcx cs (< (set-count cs) (set-count pcx)))
      (let ((cpcx (concepts->predicates cpo cs)))
        (set-intersect cpcx pcx))
      pcx))

  (let loop ((pending edges))
    (unless (null? pending)
      (define edges (sort pending edge<?))
      (define edge (car edges))
      (match-define `(,e ,s ,o) edge)
      (set!/add concept=>edges s edge)
      (set!/add concept=>edges o edge)
      (define ss  (concept->set s))
      (define os  (concept->set o))
      (define pcx (concept-predicate-intersect
                    object-predicateo
                    (concept-predicate-intersect
                      subject-predicateo (predicate->cx e) ss)
                    os))
      (define (find-edges/db db stream-edges srcs pids cats dsts)
        (define srcids
          (sort
            (if srcs srcs
              (run* (src)
                (fresh (cui cname catid catname cprops)
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

(define (duplicates xs)
  (remove-duplicates
    (let loop ((xs xs))
      (cond ((null? xs)                 '())
            ((member (car xs) (cdr xs)) (cons (car xs) (loop (cdr xs))))
            (else                       (loop (cdr xs)))))))
(define (constraints? xs)
  (or (not xs) (and (pair? xs)
                    (not (list? (cdr (car xs))))
                    (not (number? (cdr (car xs)))))))
(define (constraints->set cxs)
  (list->set (map (lambda (c) (cons (car c) (cadr c))) cxs)))
(define (constraint-sets kvs)
  (make-immutable-hash
    (map (match-lambda ((cons name cxs) (cons name (constraints->set cxs))))
         (filter (match-lambda
                   ((cons name value) (and value (constraints? value))))
                 kvs))))
(define (path*->edges paths cnames enames)
  (when (null? paths) (error "no paths were provided"))
  (unless (list? paths) (error "paths must be given as a list:" paths))
  (for-each
    (lambda (path)
      (unless (list? path) (error "path must be a list:" path))
      (unless (<= 3 (length path))
        (error "path must contain at least one edge triple:" path))
      (let loop ((parts path))
        (cond ((null? parts) (error "missing concept at end of path:" path))
              ((not (member (car parts) cnames))
               (error "unknown path concept:"
                      path `(unknown-concept: ,(car parts))
                      `(valid-concepts: ,cnames)))
              ((and (pair? (cdr parts)) (not (member (cadr parts) enames)))
               (error "unknown path edge:"
                      path `(unknown-edge: ,(cadr parts))
                      `(valid-edges: ,enames)))
              ((pair? (cdr parts)) (loop (cddr parts))))))
    paths)
  (define (path->edges path)
    (if (null? (cdr path)) '()
      (cons (list (cadr path) (car path) (caddr path))
            (path->edges (cddr path)))))
  (append* (map path->edges paths)))
(define (report-concept c)
  (define dbname (car c))
  (define cid (cdr c))
  (car (run* (concept)
          (fresh (cui name catid cat props)
            (== concept `(,dbname ,cid ,cui ,name (,catid . ,cat) ,props))
            (concepto concept)))))
(define (report-edge e)
  (define dbname (car e))
  (define eid (cadr e))
  (define scid (caddr e))
  (define ocid (cadddr e))
  (car (run* (edge)
          (fresh (scui sname scatid scat sprops
                      ocui oname ocatid ocat oprops
                      pid pred eprops)
            (== edge `(,dbname ,eid
                       (,scid ,scui ,sname (,scatid . ,scat) ,sprops)
                       (,ocid ,ocui ,oname (,ocatid . ,ocat) ,oprops)
                       (,pid . ,pred) ,eprops))
            (edgeo edge)))))
(define (report report-x x=>set)
  (make-immutable-hash
    (map (lambda (kv)
            (define name (car kv))
            (cons name (map report-x (set->list (cdr kv)))))
          (hash->list x=>set))))
(define-syntax run/graph
  (syntax-rules ()
    ((_ ((cname cexpr) ...) ((ename pexpr) ...) path* ...)
     (let* ((concepts (list (cons 'cname cexpr) ...))
            (pedges   (list (cons 'ename pexpr) ...))
            (paths    '(path* ...))
            (cnames   (map car concepts))
            (enames   (map car pedges))
            (dup-concepts (duplicates cnames))
            (dup-pedges   (duplicates enames))
            (_ (unless (null? dup-concepts)
                 (error "duplicate concept bindings:"
                        dup-concepts '((cname cexpr) ...))))
            (_ (unless (null? dup-pedges)
                 (error "duplicate edge bindings:"
                        dup-pedges '((ename pexpr) ...))))
            (edges (path*->edges paths cnames enames))
            (concept=>set
              (make-immutable-hash
                (map (match-lambda
                       ((cons name concepts)
                        (cons name (list->set
                                     (map (lambda (c)
                                            (cons (car c)
                                                  (if (number? (cdr c)) (cdr c)
                                                    (cadr c))))
                                          concepts)))))
                     (filter
                       (match-lambda
                         ((cons name value) (not (constraints? value))))
                       concepts))))
            (concept=>cx (constraint-sets concepts))
            (predicate=>cx (constraint-sets pedges)))
       (match-define (cons concept-sets edge=>set)
                     (find-graph concept=>set concept=>cx predicate=>cx edges))
       (list (report report-concept concept-sets)
             (report report-edge    edge=>set))))))
