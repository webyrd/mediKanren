#lang racket
(provide
  appendo
  decreases
  decreases*
  increases
  increases*
  lengtho
  length>=o
  not-nilo
  path-last-edgeo
  edge-subject-typeo
  edge-object-typeo
  edge-trustedo
  edge-untrustedo
  path-length-trustedo
  isa-counto
  may-treat
  gene-causes-disease
  pubmed->url
  edge/urlo
  path/urlo
  )

(require
  racket/sandbox
  racket/gui/base
  racket/engine
  "db.rkt"
  "mk-db.rkt"
  "concept.rkt"
  "edge.rkt"
  )

(displayln
  "Finished loading mk-db.rkt")

(define (pubmed->url pubmed-id)
  (string-append "https://www.ncbi.nlm.nih.gov/pubmed/" (~a pubmed-id)))

;; Warning: not relational in e
(define (edge/urlo e e-url)
  (fresh (s o p sty oty r* url*)
    (== `(,s ,o ,p ,sty ,oty ,r*) e)
    (project (r*)
      (== (map pubmed->url r*) url*))
    (== `(,s ,o ,p ,sty ,oty ,url*) e-url)))

;; Warning: not relational in p
(define (path/urlo p p-url)
  (conde
    ((== '() p) (== '() p-url))
    ((fresh (e d e-url d-url)
       (== (cons e d) p)
       (== (cons e-url d-url) p-url)
       (edge/urlo e e-url)
       (path/urlo d d-url)))))

(define (appendo l s out)
  (conde
    [(== '() l) (== s out)]
    [(fresh (a d res)
       (== (cons a d) l)
       (== (cons a res) out)
       (appendo d s res))]))

;; Warning: not relational in n (length)
(define (lengtho path n)
  (conde
    ((== 0 n) (== '() path))
    ((=/= 0 n)
     (fresh (a d)
       (== `(,a . ,d) path)
       (lengtho d (- n 1))))))

;; Warning: not relational in n (length)
(define (length>=o path n)
  (conde
    ((== 0 n))
    ((=/= 0 n)
     (fresh (a d)
       (== `(,a . ,d) path)
       (length>=o d (- n 1))))))

(define (not-nilo l) (fresh (a d) (== `(,a . ,d) l)))

;; Warning: path must be length-instantiated.
(define (path-last-edgeo path last)
  (project (path)
    (== (car (reverse path)) last)))

(define (edge-subject-typeo e sty)
  (fresh (s o pred oty ps)
    (== `(,s ,o ,pred ,sty ,oty ,ps) e)))

(define (edge-object-typeo e oty)
  (fresh (s o pred sty ps)
    (== `(,s ,o ,pred ,sty ,oty ,ps) e)))

(define (edge-trustedo edge)
  (fresh (s o p st ot pubrefs)
    (== `(,s ,o ,p ,st ,ot ,pubrefs) edge)
    (length>=o pubrefs 2)))

(define (edge-untrustedo edge)
  (fresh (s o p st ot pubrefs)
    (== `(,s ,o ,p ,st ,ot ,pubrefs) edge)
    (lengtho pubrefs 1)))

;; Warning: not relational in nlen or ntrusted
(define (path-length-trustedo path len trusted)
  (conde
    ((== 0 len) (== '() path))
    ((=/= 0 len)
     (fresh (a d)
       (== `(,a . ,d) path)
       (cond
         ((= 0 trusted) (fresh ()
                          (edge-untrustedo a)
                          (path-length-trustedo d (- len 1) trusted)))
         ((= len trusted) (fresh ()
                            (edge-trustedo a)
                            (path-length-trustedo d (- len 1) (- trusted 1))))
         (else (conde
                 ((edge-trustedo a)
                  (path-length-trustedo d (- len 1) (- trusted 1)))
                 ((edge-untrustedo a)
                  (path-length-trustedo d (- len 1) trusted)))))))))

;; Warning: not relational in o
(define (isa-counto o n)
  (project (o)
    (== (length (run* (_)
                  (fresh (s sty oty rs)
                    (edgeo `(,s ,o "ISA" ,sty ,oty ,rs))))) n)))

;; I’m having some fun defining relations that find that “a ultimately decreases b” and “a ultimately increases b” since you can have many ups and downs in the intermediate path.
;; It’s inspired by this generic example:
;; may_treat(drug,disease) iff
;;     causes(gene,disease) and
;;     increases(disease,gene) and
;;     decreases*(drug,gene)
;; I had to define decreases* mutually recursively with increases*:
;; decreases*(a,b) iff
;;    decreases(a,b)
;;    or there exists c such that
;;        increases*(a,c) and
;;        decreases*(c,b)
;; increases*(a,b) iff
;;    increases(a,b)
;;    or there exists c such that
;;        decreases*(a,c) and
;;        increases*(c,b)
;; I’m trying to emphasize the power of programmatically-defined relations.

(define (may-treat drug disease explanation)
  (fresh (gene e1 e2 p1)
    (gene-causes-disease gene disease e1)
    (increases disease gene e2)
    (decreases* drug gene p1)
    (== `((drug: ,drug)
          (gene: ,gene)
          (disease: ,disease)
          (gene-causes-disease: ,e1)
          (increases-disease/gene: ,e2)
          (decreases*-drug/gene: . ,p1))
        explanation)))

(define (gene-causes-disease gene disease edge)
  (fresh (t-gene t-disease pub)
    (== `(,gene ,disease "CAUSES" ,t-gene ,t-disease  ,pub) edge)
    (edgeo edge)
    (fresh (cui name concept-type*)
      (== `(,cui ,name ,concept-type*) gene)
      (membero "gngm" concept-type*))
    (fresh (cui name concept-type*)
      (== `(,cui ,name ,concept-type*) disease)
      (membero "dsyn" concept-type*))))

(define (decreases a b edge)
  (fresh (pred ta tb pub)
    (== `(,a ,b ,pred ,ta ,tb ,pub) edge)
    (== "INHIBITS" pred)
    (edgeo edge)))

(define (increases a b edge)
  (fresh (pred ta tb pub)
    (== `(,a ,b ,pred ,ta ,tb ,pub) edge)
    (== "STIMULATES" pred)
    (edgeo edge)))

;; decrease, increase decreasers, decrease increasers
(define (decreases* a b path)
  (conde
    [(fresh (e)
       (== `(,e) path)
       (decreases a b e))]
    [(fresh (c e p2)
       (== `(,e . ,p2) path)
       (not-nilo p2)
       (increases a c e)
       (decreases* c b p2))]
    [(fresh (c e p2)
       (== `(,e . ,p2) path)
       (not-nilo p2)
       (decreases a c e)
       (increases* c b p2))]))

;; increase, increase increasers, decrease decreasers
(define (increases* a b path)
  (conde
    [(fresh (e)
       (== `(,e) path)
       (increases a b e))]
    [(fresh (c e p2)
       (== `(,e . ,p2) path)
       (not-nilo p2)
       (increases a c e)
       (increases* c b p2))]
    [(fresh (c e p2)
       (== `(,e . ,p2) path)
       (not-nilo p2)
       (decreases a c e)
       (decreases* c b p2))]))
