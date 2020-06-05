#lang racket/base
(provide group-curie group-curies group-categories group-concepts
         concept/any concept/category concept/curie-filter concept/curie
         edge/predicate run!)
(require "synonymize.rkt"
         racket/function racket/list (except-in racket/match ==) racket/set)

;; Simple OO
;; TODO: () should summarize supported messages
(define (method-unknown name . args) (error "unknown method:" name args))
(define (method-except m names)
  (lambda (name . args)
    (apply (if (member name names) method-unknown m) name args)))
(define (method-only m names)
  (lambda (name . args)
    (apply (if (member name names) m method-unknown) name args)))
(define-syntax method-choose
  (syntax-rules (else)
    ((_ ((name ...) body ...) ... (else else-body ...))
     (lambda (method-name . args)
       (apply (case method-name
                ((name ...) body ...) ...
                (else       else-body ...))
              method-name args)))
    ((_ body ...) (method-choose body ... (else method-unknown)))))
(define-syntax method-lambda
  (syntax-rules (else)
    ((_ ((name . param) body ...) ... (else else-body ...))
     (method-choose ((name) (lambda (_ . param) body ...)) ... (else else-body ...)))
    ((_ body ...) (method-lambda body ... (else method-unknown)))))

;; TODO: priorities change and propagators are redundantly scheduled (though
;; that might mitigate some of the priority changes).  Fix these issues by
;; having propagators track their own scheduling and cost changes.
;; Heap as priority queue
(define (heap-top h) (vector-ref h 0))
(define (heap! ? h end)
  (let loop ((i (- (quotient end 2) 1)))
    (when (<= 0 i) (heap-sink! ? h end i) (loop (- i 1)))))
(define (heap-remove! ? h end)
  (vector-set! h 0 (vector-ref h (- end 1))) (heap-sink! ? h (- end 1) 0))
(define (heap-replace! ? h end top)
  (vector-set! h 0 top)                      (heap-sink! ? h    end    0))
(define (heap-sink! ? h end i)
  (let loop ((i i))
    (let ((ileft (+ i i 1)) (iright (+ i i 2)))
      (cond ((<= end ileft))  ;; done
            ((<= end iright)
             (let ((p (vector-ref h i)) (l (vector-ref h ileft)))
               (when (? l p) (vector-set! h i l) (vector-set! h ileft p))))
            (else (let ((p (vector-ref h i))
                        (l (vector-ref h ileft)) (r (vector-ref h iright)))
                    (cond ((? l p) (cond ((? r l) (vector-set! h i r)
                                                  (vector-set! h iright p)
                                                  (loop iright))
                                         (else (vector-set! h i l)
                                               (vector-set! h ileft p)
                                               (loop ileft))))
                          ((? r p) (vector-set! h i r)
                                   (vector-set! h iright p)
                                   (loop iright)))))))))
(define (heap-add! ? h end v)
  (let loop ((i end))
    (if (= i 0) (vector-set! h i v)
      (let* ((iparent (- (quotient (+ i 1) 2) 1))
             (pv      (vector-ref h iparent)))
        (cond ((? v pv) (vector-set! h i pv) (loop iparent))
              (else     (vector-set! h i v)))))))

;; Propagators
(define (p<? a b) (< ((car a)) ((car b))))
(define pending-propagators (make-vector 100))
(define pending-count 0)
(define (pending-grow!)
  (define len (vector-length pending-propagators))
  (when (= pending-count len)
    (define pending pending-propagators)
    (set! pending-propagators (make-vector (* 2 len)))
    (vector-copy! pending-propagators 0 pending 0 len)))
(define (pending-add! p)
  (pending-grow!)
  (heap-add! p<? pending-propagators pending-count p)
  (set! pending-count (+ pending-count 1)))
(define (pending-pop!)
  (define p (heap-top pending-propagators))
  (heap-remove! p<? pending-propagators pending-count)
  (set! pending-count (- pending-count 1))
  (cdr p))
(define (run!) (unless (= pending-count 0)
                 ((pending-pop!))
                 (run!)))

(define (cell =? initial-value)
  (define observers '())
  (define value     initial-value)
  (method-lambda
    ((force)           (run!) value)
    ((ref)             value)
    ((set! v)          (unless (=? v value)
                         (set! value v)
                         (for-each pending-add! observers)))
    ((observer-add! p) (set! observers (cons p observers)))))

(define (propagator dependencies cost op)
  (define p (cons cost op))
  (for-each (lambda (d) (d 'observer-add! p)) dependencies)
  (pending-add! p))

;; Graph propagation
(define (string-min a b) (if (string<? a b) a b))
(define (group-curie      g) (g 'curie))
(define (group-curies     g) (g 'curies))
(define (group-categories g) (g 'categories))
(define (group-concepts   g) (g 'concepts))
(define (group<? ga gb) (string<? (group-curie ga) (group-curie gb)))
(define (group=? ga gb) (string=? (group-curie ga) (group-curie gb)))
(define (group-member? g curie) (set-member? (group-curies g) curie))
(define (curie->group curie)
  (define synonyms (curie-synonyms curie))
  (define curies (set->list synonyms))
  (define first-curie (foldl string-min (car curies) (cdr curies)))
  (define cs         #f)
  (define categories #f)
  (define (force!)
    (unless cs
      (set! cs (find-concepts #t (set->list synonyms)))
      (set! categories
        (list->set (map cons (map car cs)
                        (map (lambda (c) (cadddr (cdr c))) cs))))))
  (method-lambda
    ((curie)      first-curie)
    ((curies)     synonyms)
    ((categories) (force!) categories)
    ((concepts)   (force!) cs)))
(define (curies->groups curies)
  (sort (cdr (foldl (lambda (c acc)
                      (let ((seen (car acc)) (groups (cdr acc)))
                        (if (set-member? seen c) acc
                          (let ((g (curie->group c)))
                            (cons (set-union seen (group-curies g))
                                  (cons g groups))))))
                    (cons (set) '())
                    curies))
        group<?))

(define (concept=? ca cb)
  (match* (ca cb)
    ((`(concept . ,as) `(concept  . ,bs))
     (equal? (list->set (map group-curie as))
             (list->set (map group-curie bs))))
    ;; TODO: if we ever find a way to reduce categories, this is not sound.
    ((`(,tag-a . ,_) `(,tag-b . ,_))
     (equal? tag-a tag-b))))

(define (concept-intersect ca cb)
  (if (eq? ca cb) ca
    (match* (ca cb)
      (('(any) _)      cb)
      ((_ '(any)) ca)
      ((`(concept . ,_) `(category     . ,_)) (concept-intersect cb ca))
      ((`(concept . ,_) `(curie-filter . ,_)) (concept-intersect cb ca))
      ((`(category . ,cats) `(concept  . ,groups))
       (define (valid-group? g)
         (ormap (lambda (cat) (set-member? (group-categories g) cat)) cats))
       `(concept . ,(filter valid-group? groups)))
      ((`(curie-filter . ,f) `(concept . ,groups))
       (define (valid-group? g) (ormap f (set->list (group-curies g))))
       `(concept . ,(filter valid-group? groups)))
      ((`(concept . ,as) `(concept  . ,bs))
       (define changed? #f)
       (define result
         (let loop ((as as) (bs bs))
           (if (or (null? as) (null? bs)) '()
             (let ((a (car as)) (b (car bs)))
               (cond ((group<? a b)
                      (set! changed? #t) (loop (cdr as)      bs))
                     ((group<? b a)
                      (set! changed? #t) (loop      as  (cdr bs)))
                     (else       (cons a (loop (cdr as) (cdr bs)))))))))
       (if changed? `(concept . ,result) ca)))))

(define (concept-constrain c curies)
  (if curies
    (match c
      ('(any) `(concept . ,(curies->groups curies)))
      (`(category . ,cats)
        (concept-intersect c `(concept . ,(curies->groups curies))))
      (`(curie-filter . ,f)
        (concept-intersect c `(concept . ,(curies->groups curies))))
      (`(concept . ,gs)
        (define (valid-group? g)
          (ormap (lambda (curie) (group-member? g curie)) curies))
        `(concept . ,(filter valid-group? gs))))
    c))

;; TODO: these should look at the object at the same time, to avoid including
;; irrelevant edges when possible.
(define (edge-constrain/subject e c)
  (match c
    ;; TODO: optionally find edges with only category/any constraints
    ('(any)               e)
    (`(category . ,_)     e)
    (`(curie-filter . ,_) e)
    (`(concept  . ,gs)
      (match e
        (`(edge . ,es)
          (define (valid-eg? e)
            (ormap (lambda (g) (group-member? g (cadr (caddr e)))) gs))
          `(edge . ,(filter valid-eg? es)))
        (`(predicate . ,ps)
          (define cs (append* (map group-concepts gs)))
          (define es (run* (e) (fresh (s o p db eid erest)
                                 (== e `(,db ,eid ,s ,o ,p . ,erest))
                                 (membero `(,db . ,s) cs)
                                 (membero `(,db . ,p) ps)
                                 (edgeo e))))
          `(edge . ,es))))))

(define (edge-constrain/object e c)
  (match c
    ;; TODO: optionally find edges with only category/any constraints
    ('(any)               e)
    (`(category . ,_)     e)
    (`(curie-filter . ,_) e)
    (`(concept  . ,gs)
      (match e
        (`(edge . ,es)
          (define (valid-eg? e)
            (ormap (lambda (g) (group-member? g (cadr (cadddr e)))) gs))
          `(edge . ,(filter valid-eg? es)))
        (`(predicate . ,ps)
          (define cs (append* (map group-concepts gs)))
          (define es (run* (e) (fresh (s o p db eid erest)
                                 (== e `(,db ,eid ,s ,o ,p . ,erest))
                                 (membero `(,db . ,o) cs)
                                 (membero `(,db . ,p) ps)
                                 (edgeo e))))
          `(edge . ,es))))))

(define (edge-subjects e)
  (match e
    (`(edge . ,es) (map (lambda (e) (cadr (caddr  e))) es))
    (_             #f)))
(define (edge-objects e)
  (match e
    (`(edge . ,es) (map (lambda (e) (cadr (cadddr e))) es))
    (_             #f)))

(define (concept-cost c)
  (match c
    (`(concept . ,gs) (foldl + 0 (map (lambda (g) (set-count (group-curies g)))
                                      gs)))
    (_                0)))
(define (edge-cost e)
  (match e
    (`(edge . ,es) (length es))
    (_             0)))

(define (concept/any) (cell concept=? '(any)))
(define (concept/category categories)
  (cell concept=? (cons 'category (find-exact-categories categories))))
(define (concept/curie curie)
  (cell concept=? (cons 'concept (list (curie->group curie)))))
(define (concept/curie-filter f) (cell concept=? (cons 'curie-filter f)))

;; TODO: db filter on edges
(define (edge/predicate predicates subject object)
  (define ps (if predicates (find-exact-predicates predicates)
               (run* (p) (predicateo p))))
  (define edge (cell equal? (cons 'predicate ps)))
  (propagator
    (list subject)
    (thunk (concept-cost (subject 'ref)))
    (thunk
     #|
      (displayln `(running subject update: ,(concept-cost (subject 'ref))
                           ,(car (subject 'ref))
                           ,(length (cdr (subject 'ref)))))
     |#
      (edge 'set! (edge-constrain/subject (edge 'ref) (subject 'ref)))))
  (propagator
    (list object)
    (thunk (concept-cost (object 'ref)))
    (thunk
     #|
      (displayln `(running object update: ,(concept-cost (object 'ref))
                           ,(car (object 'ref))
                           ,(length (cdr (object 'ref)))))
     |#
      (edge 'set! (edge-constrain/object  (edge 'ref) (object  'ref)))))
  (propagator
    (list edge)
    (thunk (edge-cost (edge 'ref)))
    (thunk
     #|
      (displayln `(running edge update: ,(edge-cost (edge 'ref))
                           ,(car (edge 'ref))))
     |#
      (define e (edge 'ref))
      (subject 'set! (concept-constrain (subject 'ref) (edge-subjects e)))
      (object  'set! (concept-constrain (object  'ref) (edge-objects  e)))))
  edge)
