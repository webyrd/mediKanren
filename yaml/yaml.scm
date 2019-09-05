(load "biolink-model.scm")
(define model biolink-model)

(define (cat l1)
  (map (lambda (c) (car c)) (cdr (assoc l1 model))))

(define (rel l1 l2)
  (map (lambda (c) (cons (car c)
                    (let ((p (assoc l2 (cdr c))))
                      (if p (cdr p) #f))))
       (cdr (assoc l1 model))))

(define (chain k r)
  (cons k
        (let ((p (assoc k r)))
          (if (cdr p)
              (chain (cdr p) r)
              '()))))
(define (refl-trans-closure r)
  (map (lambda (p) (chain (car p) r)) r))

(define categories-is-a (rel "classes" "is_a"))
(define categories-subs (refl-trans-closure categories-is-a))

(define predicates-is-a (rel "slots" "is_a"))
(define predicates-subs (refl-trans-closure predicates-is-a))

(define predicates-domain (rel "slots" "domain"))

(define predicates-range (rel "slots" "range"))

(define (mk-sub name subs)
  `(define (,name s u)
     (conde
       ,@(map (lambda (cs) `((== s ,(car cs))
                        ,`(conde
                            ,@(map (lambda (c) `((== u ,c))) cs))))
              subs))))

(load "../code/mk/mk.scm")
(load "../code/mk/test-check.scm")
(eval (mk-sub 'categories-subo categories-subs))
(eval (mk-sub 'predicates-subo predicates-subs))

(test "sub-1"
  (run* (q) (categories-subo q "pathway"))
  '(("pathways")))
(test "sub-2"
  (run* (q) (categories-subo q "gene or gene product"))
  '(("gene or gene product")
    ("gene") ("gene product") ("protein") ("gene product isoform")
    ("protein isoform") ("RNA product") ("RNA product isoform")
    ("noncoding RNA product") ("microRNA")))
(test "super-1"
  (run* (q) (categories-subo "protein" q))
  '(("protein")
    ("gene product") ("gene or gene product") ("macromolecular machine")
    ("genomic entity") ("molecular entity")
    ("biological entity") ("named thing")))
(test "super-2"
  (run* (q) (predicates-subo "regulates" q))
  '(("regulates") ("affects") ("related to")))
(test "super-3"
  (run* (q) (predicates-subo q "homologous to"))
  '(("homologous to")
    ("paralogous to")
    ("orthologous to")
    ("xenologous to")))

(define (mk-valid-type name name_csubo name_psubo)
  `(define (,name subject verb object)
     (conde
       ,@(map (lambda (pd pr)
                (assert (equal? (car pd) (car pr)))
                (filter (lambda (x) x)
                        (list
                         `(== verb ,(car pd))
                         (if (cdr pd) `(== subject ,(cdr pd)) #f)
                         (if (cdr pr) `(== object ,(cdr pr)) #f))))
              predicates-domain predicates-range))))

(eval (mk-valid-type 'valid-typeo 'categories-subo 'predicates-subo))

(test "valid-1"
  (run* (q) (valid-typeo "pathway" "regulates" "disease"))
  '((_.0)))

(test "valid-2"
  (run* (q p) (valid-typeo q "homologous to" p))
  '(((_.0 _.1))))

(test "valid-sub-1"
  (run* (s v o)
    (predicates-subo v "homologous to")
    (valid-typeo s v o))
  '(((_.0 "homologous to" _.1))
    ((_.0 "paralogous to" _.1))
    ((_.0 "orthologous to" _.1))
    ((_.0 "xenologous to" _.1))))

(test "valid-sub-2"
  (run* (s v o)
    (predicates-subo "homologous to" v)
    (valid-typeo s v o))
  '(((_.0 "homologous to" _.1))
    (("named thing" "related to" "named thing"))))

(test "valid-sub-3"
  (run* (s s1 v o o1)
    (predicates-subo v "genetically interacts with")
    (valid-typeo s v o)
    (categories-subo s1 s)
    (categories-subo o1 o))
  '((("gene" "gene" "genetically interacts with" "gene" "gene"))))

(test "valid-4"
  (run* (s v o)
    (== "molecularly interacts with" v)
    (valid-typeo s v o))
  '((("molecular entity"
    "molecularly interacts with"
    "molecular entity"))))

(test "valid-sub-4"
  (length (run* (s v o o1)
            (== "molecularly interacts with" v)
            (valid-typeo s v o)
            (categories-subo o1 o)))
  26)

(test "valid-sub-5"
  (run* (s v o o1)
    (== "molecularly interacts with" v)
    (== o1 "sequence variant")
    (valid-typeo s v o)
    (categories-subo o1 o))
  '((("molecular entity"
      "molecularly interacts with"
      "molecular entity"
      "sequence variant"))))
