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
                        ,(if (not (null? (cdr cs)))
                             `(conde
                                ,@(map (lambda (c) `((== u ,c))) (cdr cs)))
                              'fail)))
              subs))))

(load "../code/mk/mk.scm")
(load "../code/mk/test-check.scm")
(eval (mk-sub 'categories-subo categories-subs))
(eval (mk-sub 'predicates-subo predicates-subs))

(test "sub-1"
  (run* (q) (categories-subo q "pathway"))
  '())
(test "sub-2"
  (run* (q) (categories-subo q "gene or gene product"))
  '(("gene") ("gene product") ("protein") ("gene product isoform")
    ("protein isoform") ("RNA product") ("RNA product isoform")
    ("noncoding RNA product") ("microRNA")))
(test "super-1"
  (run* (q) (categories-subo "protein" q))
  '(("gene product") ("gene or gene product") ("macromolecular machine")
    ("genomic entity") ("molecular entity")
    ("biological entity") ("named thing")))
(test "super-2"
  (run* (q) (predicates-subo "regulates" q))
  '(("affects") ("related to")))


