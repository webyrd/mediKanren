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

(test "mol-sub-1"
  (length (run* (q)
            (fresh (x)
              (== "molecular entity" x)
              (=/= x q)
              (categories-subo q x))))
  25)

(test "gtga-1"
  (run* (q)
    (fresh (x)
      (== "gene to gene association" x)
      (categories-subo x q)))
  '(("gene to gene association")
    ("association")))



;;; "homologous to" should inherit the range and domain
;;;
;;; "homologous to" "is_a" "related to"
;;;
;;; related to
;;;
;;; (
;;;   "domain"
;;;   . "named thing"
;;; )
;;; (
;;;   "range"
;;;   . "named thing"
;;; )
;;; (
;;;   "multivalued"
;;;   . #t
;;; )


;;; Broken!
;;; q and p should both be "named thing",
;;; since "homologous to" is_a "related to",
;;; which has actual domain and range
(test "broken-1"
  (run* (q p)
    (valid-typeo q "homologous to" p))
  '(((_.0 _.1))))

;;; Okay
(test "broken-2"
  (run* (q)
    (fresh (pred)
      (== "homologous to" pred)
      (=/= pred q)
      (predicates-subo pred q)))
  '(("related to")))

;;; Okay
(test "broken-3"
  (run* (x y)
    (valid-typeo x "related to" y))
  '((("named thing" "named thing"))))

;;; Okay
(test "broken-4"
  (run* (q) (categories-subo "named thing" q))
  '(("named thing")))

;;; Broken!
;;; ((_.0 _.1)) is bogus, since we want only the single most specific type.
(test "broken-5"
  (run* (q p)
    (fresh (r)
      (predicates-subo "homologous to" r)
      (valid-typeo q r p)))
  '(((_.0 _.1))
    (("named thing" "named thing"))))

#!eof

;;; Tricky case from Chris Mungall:
gene_to_gene_association
   is_a association
   defining_slots [ subject : gene
                    object : gene ]
   slot_usage [ subject.range = gene
                object.range = gene ]



x = association
x.subject = protein_coding_gene
x.object = protein_coding_gene

protein_coding_gene is_a gene

=>  (implies)

gene_to_gene_association(x)



(test "Chris-1"
  (run* (pred)
    (most-specific-predo "protein coding gene" "association" "protein coding gene" pred))
  '((("gene to gene association"))))

