#lang racket

(provide
 (all-from-out "mk.rkt")
 (all-from-out "db.rkt")
 (all-from-out "mk-db.rkt")
 
 (all-defined-out))

(require
  "mk.rkt"
  "db.rkt"
  "mk-db.rkt"
  (except-in racket/match ==))

(displayln "loading semmed")
(define semmed (time (make-db "data/semmed")))
;; (displayln "loading monarch-lite")
;; (define monarch (time (make-db "data/monarch-lite")))
;; (displayln "loading rtx")
;; (define rtx (time (make-db "data/rtx")))
;; (displayln "loading scigraph")
;; (define scigraph (time (make-db "data/scigraph")))

;; list membership
(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        [(== x y)]
        [(=/= x y) (membero x rest)]))))

;; remove duplicates from a list
(define rem-dups
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(member (car ls) (cdr ls)) (rem-dups (cdr ls))]
      [else (cons (car ls) (rem-dups (cdr ls)))])))


(define (edgeo e)
  (conde
    ((fresh (ee) (== `(semmed . ,ee) e) (db:edgeo semmed ee)))
    ;((fresh (ee) (== `(monarch . ,ee) e) (db:edgeo monarch ee)))
    ;((fresh (ee) (== `(rtx . ,ee) e) (db:edgeo rtx ee)))
    ;((fresh (ee) (== `(scigraph . ,ee) e) (db:edgeo scigraph ee)))
    ))

(define (~name-concepto n c)
  (conde
    ((fresh (cc) (== `(semmed . ,cc) c) (db:~name-concepto semmed n cc)))
    ;((fresh (cc) (== `(monarch . ,cc) c) (db:~name-concepto monarch n cc)))
    ;((fresh (cc) (== `(rtx . ,cc) c) (db:~name-concepto rtx n cc)))
    ;((fresh (cc) (== `(scigraph . ,cc) c) (db:~name-concepto scigraph n cc)))
    ))

;; Greg's examples from example-semmed.rkt
(displayln "associating pubmed ids with edge ids:")
(time (pretty-print (run 10 (pmid eid) (db:pmid-eido semmed pmid eid))))
(time (pretty-print (run*        (eid) (db:pmid-eido semmed "1000085" eid))))
(time (pretty-print (run*        (eid) (db:pmid-eido semmed "10000" eid))))
(time (pretty-print (run*        (eid) (db:pmid-eido semmed "999999" eid))))


(run 1 (e)
  (fresh (dbname eid rest)
    (db:pmid-eido semmed "1000085" eid)
    (== `(,dbname ,eid . ,rest) e)
    (edgeo e)))

(run* (e)
  (fresh (dbname eid rest)
    (db:pmid-eido semmed "1000085" eid)
    (== `(,dbname ,eid . ,rest) e)
    (edgeo e)))

(run* (q)
  (fresh (dbname eid e
                 scid scui sname scatid scat sprops
                 ocid ocui oname ocatid ocat oprops
                 pid pred eprops)
    (db:pmid-eido semmed "1000085" eid)
    (== `(,dbname ,eid (,scid ,scui ,sname (,scatid . ,scat) . ,sprops)
                  (,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops)
                  (,pid . ,pred) . ,eprops)
        e)
    (== (list sname pred oname) q)
    (edgeo e)))

(define find-edges-by-pubmed-id
  (lambda (pubmed-id)
    (let ((pubmed-id-str (if (string? pubmed-id)
                             pubmed-id
                             (~a pubmed-id))))
      (printf "finding edges for PubMed entry ~s\n" pubmed-id-str)
      (run* (q)
        (fresh (dbname eid e
                       scid scui sname scatid scat sprops
                       ocid ocui oname ocatid ocat oprops
                       pid pred eprops)
          (db:pmid-eido semmed pubmed-id-str eid)
          (== `(,dbname ,eid (,scid ,scui ,sname (,scatid . ,scat) . ,sprops)
                        (,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops)
                        (,pid . ,pred) . ,eprops)
              e)
          (== (list sname pred oname eid) q)
          (edgeo e))))))

(define find-pubmed-ids-by-eid
  (lambda (eid)
    (printf "finding PubMed Ids that support edge ~s\n" eid)
    (let ((ans (run* (q)
                 (fresh (dbname e
                                scid scui sname scatid scat sprops
                                ocid ocui oname ocatid ocat oprops
                                pid pred eprops)
                   (== 'semmed dbname)
                   (== `(,dbname ,eid (,scid ,scui ,sname (,scatid . ,scat) . ,sprops)
                                 (,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops)
                                 (,pid . ,pred) . ,eprops)
                       e)
                   (edgeo e)
                   (project (eprops)
                     (let ((pmid-pr (assoc "pmids" eprops)))
                       (cond
                         [pmid-pr
                          (let ((pmid-str* (string-split (cdr pmid-pr) ";")))
                            (== pmid-str* q))]
                         [else
                          (printf "bad edge: ~s\n" e)
                          (printf "bad properties: ~s\n" eprops)
                          (printf "bad pmid-pr: ~s\n" pmid-pr)
                          (error 'loop "missing pmids entry")])))))))
      (if (null? ans)
          '()
          (sort (rem-dups (map string->number (car ans))) <)))))

; (find-pubmed-ids-by-eid 4759781)
; (find-edges-by-pubmed-id 28230014)

(map find-edges-by-pubmed-id (find-pubmed-ids-by-eid 4759781))

(define find-edges-related-to-given-edge
  (lambda (eid)
    (printf "finding edges supported by publications that support edge ~s\n" eid)
    (sort
     (rem-dups (foldr (lambda (pmid l)
                        (append (find-edges-by-pubmed-id pmid) l))
                      '()
                      (find-pubmed-ids-by-eid eid)))
     (lambda (l1 l2)
       (let ((o1 (string-downcase (car l1)))
             (o2 (string-downcase (car l2)))
             (p1 (string-downcase (cadr l1)))
             (p2 (string-downcase (cadr l2)))
             (s1 (string-downcase (caddr l1)))
             (s2 (string-downcase (caddr l2))))
         (or (string<? p1 p2)
             (and (string=? p1 p2)
                  (string<? o1 o2))
             (and (string=? p1 p2)
                  (string=? o1 o2)
                  (string<? s1 s2))))))))

(find-edges-related-to-given-edge 4759781)

(db:eid->edge semmed 5337181)

(let ((v (db:eid->edge semmed 5337181)))
  (let ((eprops (vector-ref v 3)))
    (let ((pmid-pr (assoc "pmids" eprops)))
      (cond
        [pmid-pr
         (let ((pmid-str* (rem-dups (string-split (cdr pmid-pr) ";"))))
           (map (lambda (p) (string-append "https://www.ncbi.nlm.nih.gov/pubmed/" p)) pmid-str*))]
        [else
         (printf "bad edge: ~s\n" v)
         (printf "bad properties: ~s\n" eprops)
         (printf "bad pmid-pr: ~s\n" pmid-pr)
         (error 'loop "missing pmids entry")]))))
