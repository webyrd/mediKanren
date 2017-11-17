#lang racket
(require
  "mk-db.rkt"
  )

(displayln
  "Finished loading mk-db.rkt.")

;; https://docs.google.com/presentation/d/12cNiuQgBbCz3bs20rzoX-D_VWzLvYMvloa7512XmpaU/edit#slide=id.g2ab4b1b7ae_0_602

;; we want a direct link bewteen imatinib and GIST
;; like TREATS predicate
;; or something like that
;; and no direct link bwteen imatinib and Asthma
;; which is what we are "discovering"
;; I think that is the basic idea
;; we already "know" that imatinib treats CML and GIST
;; through direct links
;; and are trying to discover other diseases it might treat
;; but want to go through genes that are already known to be safe
;; that imatinib is already known to target

;; the example is interesting because the query isn't just linear
;; we want to find a tree, not a path

;; Julian's SPARQL query:
;;
;; select ?activation ?disease where {
;;  :imatinib :inhibits ?gene .
;;  ?gene :associatedWith ?activation .
;;  ?activation :associatedWith ?disease
;;  FILTER EXISTS {
;;    ?gene :associatedWith ?activation2 .
;;    ?activation2 :associatedWith ?disease2 .
;;    :imatinib :treats ?disease2
;;  }
;;  FILTER NOT EXISTS {
;;    :imatinib :treats ?disease
;;  }
;; }


"ASSOCIATED_WITH"
"AFFECTS"

(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        [(== x y)]
        [(=/= x y) (membero x rest)]))))

(define not-membero
  (lambda (x ls)
    (conde
      [(== '() ls)]
      [(fresh (y rest)
         (== `(,y . ,rest) ls)
         (=/= x y)
         (not-membero x rest))])))

(define path-to-diseaseo
  (lambda (x path)
    (fresh (cui name concept-type*)
      (== `(,cui ,name ,concept-type*) x)
      (conde
        [(membero "dsyn" concept-type*)
         (== `(,x) path)]
        [(not-membero "dsyn" concept-type*)
         (fresh (y p e e-rest path^)
           (== `(,e . ,path^) path)
           (== `(,x ,y ,p . ,e-rest) e)
           (conde
             [(== "AFFECTS" p)]
             [(== "CAUSES" p)])
           (edgeo e)
           (path-to-diseaseo y path^))]))))


(run* (possible-treatment)
  (fresh (drug gene disease1 disease2)
    (fuzzy-match "imatinib" drug)
    (inhibits-gene drug gene)
    (causes-disease gene disease1 mechanism1)
    (treats-disease drug disease1)
    (causes-disease gene disease2 mechanism2)
    (unknown-if-treats-disease drug disease2)
    (== (list drug gene mechanism2 disease2) possible-treatment)))




(run 1 (q)
  (fresh (drug gene known-disease something unknown-disease
          e-drug/gene p-drug/gene e-drug/gene-rest
          e-gene/known-disease p-gene/known-disease e-gene/known-disease-rest
          e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
          e-gene/something p-gene/something e-gene/something-rest
          e-something/unknown-disease p-something/unknown-disease e-something/unknown-disease-rest)
          
    (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)
                  
    ;; imatinib inhibits some gene
    (fuzzy-concepto "imatinib" drug)
    (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
    (== "INHIBITS" p-drug/gene)
    (edgeo e-drug/gene)
    (fresh (cui name concept-type*)
      (== `(,cui ,name ,concept-type*) gene)
      (membero "gngm" concept-type*))

    ;; that gene directly causes some disease...
    (== `(,gene ,known-disease ,p-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
    (== "CAUSES" p-gene/known-disease)
    (edgeo e-gene/known-disease)
    (fresh (cui name concept-type*)
      (== `(,cui ,name ,concept-type*) known-disease)
      (conde
        [(membero "dsyn" concept-type*)]
        [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))

    ;; ...which imatinib is known to treat
    (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
    (== "TREATS" p-drug/known-disease)
    (edgeo e-drug/known-disease)                  
                   
    ;; and that gene indirectly causes & indirectly affects some other disease
    (== `(,gene ,something ,p-gene/something . ,e-gene/something-rest) e-gene/something)
    (== `(,something ,unknown-disease ,p-something/unknown-disease . ,e-something/unknown-disease-rest) e-something/unknown-disease)
    (== "CAUSES" p-gene/something)
    (conde
      [(== "AFFECTS" p-something/unknown-disease)]
      [(== "CAUSES" p-something/unknown-disease)])
    (edgeo e-gene/something)
                  
    (fresh (cui name concept-type*)
      (== `(,cui ,name ,concept-type*) something)
      (not-membero "dsyn" concept-type*)
      (not-membero "neop" concept-type*))

    (edgeo e-something/unknown-disease)
                                    
    (fresh (cui name concept-type*)
      (== `(,cui ,name ,concept-type*) unknown-disease)
      (conde
        [(membero "dsyn" concept-type*)]
        [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))
    ))









(time (run 1 (q)
        (fresh (drug gene known-disease something unknown-disease
                     e-drug/gene p-drug/gene e-drug/gene-rest
                     e-gene/known-disease p-gene/known-disease e-gene/known-disease-rest
                     e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
                     e-gene/something p-gene/something e-gene/something-rest
                     e-something/unknown-disease p-something/unknown-disease e-something/unknown-disease-rest)
          
          (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)
                  
          ;; imatinib 
          (== '(935989 "imatinib" ("phsu" "orch")) drug)

          ;; ...which imatinib is known to treat
          (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
          (== "TREATS" p-drug/known-disease)
          (edgeo e-drug/known-disease)                  

          ;; imatinib inhibits some gene
          (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
          (== "INHIBITS" p-drug/gene)
          (edgeo e-drug/gene)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) gene)
            (membero "gngm" concept-type*))

          ;; that gene directly causes some disease...
          (== `(,gene ,known-disease ,p-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
          (== "CAUSES" p-gene/known-disease)
          (edgeo e-gene/known-disease)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) known-disease)
            (conde
              [(membero "dsyn" concept-type*)]
              [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))
                   
          ;; and that gene indirectly causes & indirectly affects some other disease
          (== `(,gene ,something ,p-gene/something . ,e-gene/something-rest) e-gene/something)
          (== `(,something ,unknown-disease ,p-something/unknown-disease . ,e-something/unknown-disease-rest) e-something/unknown-disease)
          (== "CAUSES" p-gene/something)

          (== "AFFECTS" p-something/unknown-disease)
          
          ;(conde
          ;  [(== "AFFECTS" p-something/unknown-disease)]
          ;  [(== "CAUSES" p-something/unknown-disease)])
          
          (edgeo e-gene/something)
                  
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) something)
            (not-membero "dsyn" concept-type*)
            (not-membero "neop" concept-type*))

          (edgeo e-something/unknown-disease)
                                    
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) unknown-disease)
            (conde
              [(membero "dsyn" concept-type*)]
              [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))


          ;; generate and test!!
          (fuzzy-concepto "KIT gene" gene)
          (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
          (fuzzy-concepto "mast cell activation" something)
          (fuzzy-concepto "asthma" unknown-disease)
          
          )))

(time (run 1 (q)
        (fresh (drug gene known-disease something unknown-disease
                     e-drug/gene p-drug/gene e-drug/gene-rest
                     e-gene/known-disease p-gene/known-disease e-gene/known-disease-rest
                     e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
                     e-gene/something p-gene/something e-gene/something-rest
                     e-something/unknown-disease p-something/unknown-disease e-something/unknown-disease-rest)
          
          (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)
                  
          ;; imatinib inhibits some gene
          (fuzzy-concepto "imatinib" drug)
          (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
          (== "INHIBITS" p-drug/gene)
          (edgeo e-drug/gene)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) gene)
            (membero "gngm" concept-type*))

          ;; that gene directly causes some disease...
          (== `(,gene ,known-disease ,p-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
          (== "CAUSES" p-gene/known-disease)
          (edgeo e-gene/known-disease)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) known-disease)
            (conde
              [(membero "dsyn" concept-type*)]
              [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))

          ;; ...which imatinib is known to treat
          (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
          (== "TREATS" p-drug/known-disease)
          (edgeo e-drug/known-disease)                  
                   
          ;; and that gene indirectly causes & indirectly affects some other disease
          (== `(,gene ,something ,p-gene/something . ,e-gene/something-rest) e-gene/something)
          (== `(,something ,unknown-disease ,p-something/unknown-disease . ,e-something/unknown-disease-rest) e-something/unknown-disease)
          (== "CAUSES" p-gene/something)
          (conde
            [(== "AFFECTS" p-something/unknown-disease)]
            [(== "CAUSES" p-something/unknown-disease)])
          (edgeo e-gene/something)
                  
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) something)
            (not-membero "dsyn" concept-type*)
            (not-membero "neop" concept-type*))

          (edgeo e-something/unknown-disease)
                                    
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) unknown-disease)
            (conde
              [(membero "dsyn" concept-type*)]
              [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))


          ;; generate and test!!, cheating  (move down for realz)
          (fuzzy-concepto "KIT gene" gene)
          ;; (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
          ;; (fuzzy-concepto "mast cell activation" something)
          (fuzzy-concepto "asthma" unknown-disease)
          
          )))

(time (run 1 (q)
        (fresh (drug gene known-disease something unknown-disease
                     e-drug/gene p-drug/gene e-drug/gene-rest
                     e-gene/known-disease p-gene/known-disease e-gene/known-disease-rest
                     e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
                     e-gene/something p-gene/something e-gene/something-rest
                     e-something/unknown-disease p-something/unknown-disease e-something/unknown-disease-rest)
          
          (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)
                  
          ;; imatinib inhibits some gene
          (fuzzy-concepto "imatinib" drug)
          (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
          (== "INHIBITS" p-drug/gene)
          (edgeo e-drug/gene)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) gene)
            (membero "gngm" concept-type*))

          ;; that gene directly causes some disease...
          (== `(,gene ,known-disease ,p-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
          (== "CAUSES" p-gene/known-disease)
          (edgeo e-gene/known-disease)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) known-disease)
            (conde
              [(membero "dsyn" concept-type*)]
              [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))

          ;; ...which imatinib is known to treat
          (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
          (== "TREATS" p-drug/known-disease)
          (edgeo e-drug/known-disease)                  
                   
          ;; and that gene indirectly causes & indirectly affects some other disease
          (== `(,gene ,something ,p-gene/something . ,e-gene/something-rest) e-gene/something)
          (== `(,something ,unknown-disease ,p-something/unknown-disease . ,e-something/unknown-disease-rest) e-something/unknown-disease)
          (== "CAUSES" p-gene/something)
          (conde
            [(== "AFFECTS" p-something/unknown-disease)]
            [(== "CAUSES" p-something/unknown-disease)])
          (edgeo e-gene/something)
                  
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) something)
            (not-membero "dsyn" concept-type*)
            (not-membero "neop" concept-type*))

          (edgeo e-something/unknown-disease)
                                    
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) unknown-disease)
            (conde
              [(membero "dsyn" concept-type*)]
              [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))


          ;; generate and test!!, cheating  (move down for realz)
          (fuzzy-concepto "KIT gene" gene)
          (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
          (fuzzy-concepto "mast cell activation" something)
          (fuzzy-concepto "asthma" unknown-disease)

          
          )))

(time (run 1 (q)
        (fresh (drug gene known-disease something unknown-disease
                     e-drug/gene p-drug/gene e-drug/gene-rest
                     e-gene/known-disease p-gene/known-disease e-gene/known-disease-rest
                     e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
                     e-gene/something p-gene/something e-gene/something-rest
                     e-something/unknown-disease p-something/unknown-disease e-something/unknown-disease-rest)

          ;; generate and test!!, cheating  (move down for realz)
          (fuzzy-concepto "KIT gene" gene)
          (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
          (fuzzy-concepto "mast cell activation" something)
          (fuzzy-concepto "asthma" unknown-disease)

          
          (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)
                  
          ;; imatinib inhibits some gene
          (fuzzy-concepto "imatinib" drug)
          (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
          (== "INHIBITS" p-drug/gene)
          (edgeo e-drug/gene)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) gene)
            (membero "gngm" concept-type*))

          ;; that gene directly causes some disease...
          (== `(,gene ,known-disease ,p-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
          (== "CAUSES" p-gene/known-disease)
          (edgeo e-gene/known-disease)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) known-disease)
            (conde
              [(membero "dsyn" concept-type*)]
              [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))

          ;; ...which imatinib is known to treat
          (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
          (== "TREATS" p-drug/known-disease)
          (edgeo e-drug/known-disease)                  
                   
          ;; and that gene indirectly causes & indirectly affects some other disease
          (== `(,gene ,something ,p-gene/something . ,e-gene/something-rest) e-gene/something)
          (== `(,something ,unknown-disease ,p-something/unknown-disease . ,e-something/unknown-disease-rest) e-something/unknown-disease)
          (== "CAUSES" p-gene/something)
          (conde
            [(== "AFFECTS" p-something/unknown-disease)]
            [(== "CAUSES" p-something/unknown-disease)])
          (edgeo e-gene/something)
                  
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) something)
            (not-membero "dsyn" concept-type*)
            (not-membero "neop" concept-type*))

          (edgeo e-something/unknown-disease)
                                    
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) unknown-disease)
            (conde
              [(membero "dsyn" concept-type*)]
              [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))

                  
          )))

(time (length (run 100 (q)
                (fresh (drug gene known-disease something unknown-disease
                        e-drug/gene p-drug/gene e-drug/gene-rest
                        e-gene/known-disease p-gene/known-disease e-gene/known-disease-rest
                        e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
                        e-gene/something p-gene/something e-gene/something-rest
                        e-something/unknown-disease p-something/unknown-disease e-something/unknown-disease-rest)

                  (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)
                  
                  ;; imatinib inhibits some gene
                  (fuzzy-concepto "imatinib" drug)
                  (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                  (== "INHIBITS" p-drug/gene)
                  (edgeo e-drug/gene)
                  (fresh (cui name concept-type*)
                    (== `(,cui ,name ,concept-type*) gene)
                    (membero "gngm" concept-type*))

                  ;; that gene directly causes some disease...
                  (== `(,gene ,known-disease ,p-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
                  (== "CAUSES" p-gene/known-disease)
                  (edgeo e-gene/known-disease)
                  (fresh (cui name concept-type*)
                    (== `(,cui ,name ,concept-type*) known-disease)
                    (conde
                      [(membero "dsyn" concept-type*)]
                      [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))

                  ;; ...which imatinib is known to treat
                  (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
                  (== "TREATS" p-drug/known-disease)
                  (edgeo e-drug/known-disease)                  
                   
                  ;; and that gene indirectly causes & indirectly affects some other disease
                  (== `(,gene ,something ,p-gene/something . ,e-gene/something-rest) e-gene/something)
                  (== `(,something ,unknown-disease ,p-something/unknown-disease . ,e-something/unknown-disease-rest) e-something/unknown-disease)
                  (== "CAUSES" p-gene/something)
                  (conde
                    [(== "AFFECTS" p-something/unknown-disease)]
                    [(== "CAUSES" p-something/unknown-disease)])
                  (edgeo e-gene/something)
                  
                  (fresh (cui name concept-type*)
                    (== `(,cui ,name ,concept-type*) something)
                    (not-membero "dsyn" concept-type*)
                    (not-membero "neop" concept-type*))

                  (edgeo e-something/unknown-disease)
                                    
                  (fresh (cui name concept-type*)
                    (== `(,cui ,name ,concept-type*) unknown-disease)
                    (conde
                      [(membero "dsyn" concept-type*)]
                      [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))
                  
                  ))))

; cpu time: 31325 real time: 31336 gc time: 490
; 6518
(time (length (run* (q)
                (fresh (drug gene known-disease
                        e-drug/gene p-drug/gene e-drug/gene-rest
                        e-gene/known-disease p-gene/known-disease e-gene/known-disease-rest
                        e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest)

                  (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease) q)
                  
                  ;; imatinib inhibits some gene
                  (fuzzy-concepto "imatinib" drug)
                  (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                  (== "INHIBITS" p-drug/gene)
                  (edgeo e-drug/gene)
                  (fresh (cui name concept-type*)
                    (== `(,cui ,name ,concept-type*) gene)
                    (membero "gngm" concept-type*))

                  ;; that gene directly causes some disease...
                  (== `(,gene ,known-disease ,p-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
                  (== "CAUSES" p-gene/known-disease)
                  (edgeo e-gene/known-disease)
                  (fresh (cui name concept-type*)
                    (== `(,cui ,name ,concept-type*) known-disease)
                    (conde
                      [(membero "dsyn" concept-type*)]
                      [(membero "neop" concept-type*)]))

                  ;; ...which imatinib is known to treat
                  (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
                  (== "TREATS" p-drug/known-disease)
                  (edgeo e-drug/known-disease)                  
                   
                  ;; and that gene directly or indirectly causes&affects some other disease
                  
                  
                  ))))

(time (length (run* (q)
                (fresh (drug gene e-drug/gene p-drug/gene e-drug/gene-rest)
                  (== '(939537 "Imatinib mesylate" ("orch" "phsu")) drug)
                  (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                  (== "INHIBITS" p-drug/gene)                  
                  (edgeo e-drug/gene)
                  (fresh (cui name concept-type*)
                    (== `(,cui ,name ,concept-type*) gene)
                    (membero "gngm" concept-type*))
                  (conde
                    [(fresh (e d p e-rest)
                       (== `(,e-drug/gene ,e) q)
                       (== `(,gene ,d ,p . ,e-rest) e)
                       (conde
                         [(== "AFFECTS" p)]
                         [(== "CAUSES" p)])
                       (edgeo e)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) d)
                         (membero "dsyn" concept-type*)))]
                    [(fresh (e e2 y z p e-rest p2 e2-rest)
                       (== `(,e-drug/gene ,e ,e2) q)
                       (== `(,gene ,y ,p . ,e-rest) e)
                       (== `(,y ,z ,p2 . ,e2-rest) e2)
                       (== "CAUSES" p)
                       (conde
                         [(== "AFFECTS" p2)]
                         [(== "CAUSES" p2)])          
                       (edgeo e)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) y)
                         (not-membero "dsyn" concept-type*))
                       (edgeo e2)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) z)
                         (membero "dsyn" concept-type*)))])))))


> (time (length (run 1000000 (q)
                (fresh (drug gene e-drug/gene p-drug/gene e-drug/gene-rest)
                  (== '(939537 "Imatinib mesylate" ("orch" "phsu")) drug)
                  (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                  (== "INHIBITS" p-drug/gene)                  
                  (edgeo e-drug/gene)
                  (fresh (cui name concept-type*)
                    (== `(,cui ,name ,concept-type*) gene)
                    (membero "gngm" concept-type*))
                  (fresh (e e2 y z p e-rest p2 e2-rest)
                       (== `(,e-drug/gene ,e ,e2) q)
                       (== `(,gene ,y ,p . ,e-rest) e)
                       (== `(,y ,z ,p2 . ,e2-rest) e2)
                       (== "CAUSES" p)
                       (== "CAUSES" p2)          
                       (edgeo e)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) y)
                         (not-membero "dsyn" concept-type*))
                       (edgeo e2)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) z)
                         (membero "dsyn" concept-type*)))))))
cpu time: 68505 real time: 69284 gc time: 7253
773840

> (time (length (run 100000 (q)
                (fresh (drug gene e-drug/gene p-drug/gene e-drug/gene-rest)
                  (== '(939537 "Imatinib mesylate" ("orch" "phsu")) drug)
                  (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                  (== "INHIBITS" p-drug/gene)                  
                  (edgeo e-drug/gene)
                  (fresh (cui name concept-type*)
                    (== `(,cui ,name ,concept-type*) gene)
                    (membero "gngm" concept-type*))
                  (conde
                    [(fresh (e d p e-rest)
                       (== `(,e-drug/gene ,e) q)
                       (== `(,gene ,d ,p . ,e-rest) e)
                       (conde
                         [(== "AFFECTS" p)]
                         [(== "CAUSES" p)])
                       (edgeo e)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) d)
                         (membero "dsyn" concept-type*)))])))))
cpu time: 1173 real time: 1190 gc time: 71
8237

> (time (length (run 100000 (q)
                (fresh (drug gene e-drug/gene p-drug/gene e-drug/gene-rest)
                  (== '(939537 "Imatinib mesylate" ("orch" "phsu")) drug)
                  (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                  (== "INHIBITS" p-drug/gene)                  
                  (edgeo e-drug/gene)
                  (fresh (cui name concept-type*)
                    (== `(,cui ,name ,concept-type*) gene)
                    (membero "gngm" concept-type*))
                  (fresh (e d p e-rest)
                       (== `(,e-drug/gene ,e) q)
                       (== `(,gene ,d ,p . ,e-rest) e)
                       (== "CAUSES" p)
                       (edgeo e)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) d)
                         (membero "dsyn" concept-type*)))))))
cpu time: 466 real time: 468 gc time: 23
5028

(time (length (run* (q)
                (fresh (drug gene e-drug/gene p-drug/gene e-drug/gene-rest)
                  (== '(939537 "Imatinib mesylate" ("orch" "phsu")) drug)
                  (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                  (== "INHIBITS" p-drug/gene)                  
                  (edgeo e-drug/gene)
                  (fresh (cui name concept-type*)
                    (== `(,cui ,name ,concept-type*) gene)
                    (membero "gngm" concept-type*))
                  (conde
                    [(fresh (e d p e-rest)
                       (== `(,e-drug/gene ,e) q)
                       (== `(,gene ,d ,p . ,e-rest) e)
                       (conde
                         [(== "AFFECTS" p)]
                         [(== "CAUSES" p)])
                       (edgeo e)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) d)
                         (membero "dsyn" concept-type*)))]
                    [(fresh (e e2 y z p e-rest p2 e2-rest)
                       (== `(,e-drug/gene ,e ,e2) q)
                       (== `(,gene ,y ,p . ,e-rest) e)
                       (== `(,y ,z ,p2 . ,e2-rest) e2)
                       (== "CAUSES" p)
                       (conde
                         [(== "AFFECTS" p2)]
                         [(== "CAUSES" p2)])          
                       (edgeo e)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) y)
                         (not-membero "dsyn" concept-type*))
                       (edgeo e2)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) z)
                         (membero "dsyn" concept-type*)))])))))

;; diseases that are directly affected or caused by gene G,
;; or diseases that are directly affected or caused by Y, where Y is any *non-disease* caused by gene G    [one level deep],
;; where gene G is directly inhibited by 
(time (length (run* (q)
                (fresh (drug gene e-drug/gene p-drug/gene e-drug/gene-rest)
                  (fuzzy-concepto "imatinib" drug)
                  (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                  (== "INHIBITS" p-drug/gene)                  
                  (edgeo e-drug/gene)
                  (fresh (cui name concept-type*)
                    (== `(,cui ,name ,concept-type*) gene)
                    (membero "gngm" concept-type*))
                  (conde
                    [(fresh (e d p e-rest)
                       (== `(,e-drug/gene ,e) q)
                       (== `(,gene ,d ,p . ,e-rest) e)
                       (conde
                         [(== "AFFECTS" p)]
                         [(== "CAUSES" p)])
                       (edgeo e)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) d)
                         (membero "dsyn" concept-type*)))]
                    [(fresh (e e2 y z p e-rest p2 e2-rest)
                       (== `(,e-drug/gene ,e ,e2) q)
                       (== `(,gene ,y ,p . ,e-rest) e)
                       (== `(,y ,z ,p2 . ,e2-rest) e2)
                       (== "CAUSES" p)
                       (conde
                         [(== "AFFECTS" p2)]
                         [(== "CAUSES" p2)])          
                       (edgeo e)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) y)
                         (not-membero "dsyn" concept-type*))
                       (edgeo e2)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) z)
                         (membero "dsyn" concept-type*)))])))))

;; diseases that are directly affected or caused by KIT gene,
;; or diseases that are directly affected or caused by Y, where Y is any *non-disease* caused by KIT gene    [one level deep]
;;
;; cpu time: 1750 real time: 1753 gc time: 294
;; 16574 total = 40 + 16534, as expected from the queries below
(time (length (run* (q)
                (fresh (x)
                  (fuzzy-concepto "KIT gene" x)
                  (conde
                    [(fresh (e d p e-rest)
                       (== `(,e) q)
                       (== `(,x ,d ,p . ,e-rest) e)
                       (conde
                         [(== "AFFECTS" p)]
                         [(== "CAUSES" p)])
                       (edgeo e)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) d)
                         (membero "dsyn" concept-type*)))]
                    [(fresh (e e2 y z p e-rest p2 e2-rest)
                       (== `(,e ,e2) q)
                       (== `(,x ,y ,p . ,e-rest) e)
                       (== `(,y ,z ,p2 . ,e2-rest) e2)
                       (== "CAUSES" p)
                       (conde
                         [(== "AFFECTS" p2)]
                         [(== "CAUSES" p2)])          
                       (edgeo e)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) y)
                         (not-membero "dsyn" concept-type*))
                       (edgeo e2)
                       (fresh (cui name concept-type*)
                         (== `(,cui ,name ,concept-type*) z)
                         (membero "dsyn" concept-type*)))])))))

;; diseases that are directly affected or caused by Y, where Y is any *non-disease* caused by KIT gene (16534)   [one level deep]
;; cpu time: 1589 real time: 1592 gc time: 81
(time (length (run* (e e2)
        (fresh (x y z p e-rest p2 e2-rest)
          (fuzzy-concepto "KIT gene" x)
          (== `(,x ,y ,p . ,e-rest) e)
          (== `(,y ,z ,p2 . ,e2-rest) e2)
          (== "CAUSES" p)
          (conde
            [(== "AFFECTS" p2)]
            [(== "CAUSES" p2)])          
          (edgeo e)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) y)
            (not-membero "dsyn" concept-type*))
          (edgeo e2)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) z)
            (membero "dsyn" concept-type*))
          ))))

;; diseases that are directly affected or caused by Y, where Y is anything caused by KIT gene (22012)   [one level deep]
;;
;; cpu time: 2251 real time: 2273 gc time: 315
#|
(time (length (run* (e e2)
        (fresh (x y z p e-rest p2 e2-rest)
          (fuzzy-concepto "KIT gene" x)
          (== `(,x ,y ,p . ,e-rest) e)
          (== `(,y ,z ,p2 . ,e2-rest) e2)
          (== "CAUSES" p)
          (conde
            [(== "AFFECTS" p2)]
            [(== "CAUSES" p2)])          
          (edgeo e)
          (edgeo e2)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) z)
            (membero "dsyn" concept-type*))
          ))))
|#

;; find diseases *directly* caused or affected by KIT gene (40)
;; cpu time: 104 real time: 106 gc time: 2
#|
(time (run* (e)
        (fresh (x d p e-rest)
          (fuzzy-concepto "KIT gene" x)
          (== `(,x ,d ,p . ,e-rest) e)
          (conde
            [(== "AFFECTS" p)]
            [(== "CAUSES" p)])
          (edgeo e)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) d)
            (membero "dsyn" concept-type*))
          )))
|#

;; diseases directly caused by or affected by mast cell activation,
;; and which are *not* directly treated by imatinib
;;
;; uh oh!  wanted to try negation as failure, but faster-mk doesn't include
;; conda or condu, and probably isn't sound to use them anyway in the presence of
;; constraints and violation of the g-rule.
;;
;; rethink!
#|
(time (run* (e-mast/disease e-imatinib/disease)
        (fresh (x d p e-rest)
          (fuzzy-concepto "mast cell activation" x)
          (== `(,x ,d ,p . ,e-rest) e-mast/disease)
          (conde
            [(== "AFFECTS" p)]
            [(== "CAUSES" p)])
          (edgeo e-mast/disease)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) d)
            (membero "dsyn" concept-type*))

          (fresh (s-imatinib/disease m-imatinib/disease p-imatinib/disease e-rest-imatinib/disease)
            (== `(,s-imatinib/disease ,m-imatinib/disease ,p-imatinib/disease . ,e-rest-imatinib/disease) e-imatinib/disease)
            (fuzzy-concepto "imatinib" s-imatinib/disease)
            (== m-imatinib/disease d)
            (== "TREATS" p-imatinib/disease)
            (conda
              [(edgeo e-imatinib/disease)
               (== #f #t) ; negation as failure -- yuck!
               ]
              [(== #f #f) ; succeed!
               ]))
          )))
|#

;; diseases directly caused by or affected by mast cell activation,
;; and which are directly treated by imatinib (12)
;; cpu time: 1495 real time: 1496 gc time: 7
'((((1155074 "mast cell activation" ("celf"))
    (3864 "Arthritis" ("dsyn"))
    "AFFECTS"
    "celf"
    "dsyn"
    (76040540))
   ((935989 "imatinib" ("phsu" "orch"))
    (3864 "Arthritis" ("dsyn"))
    "TREATS"
    "phsu"
    "dsyn"
    (80123007 63539282)))
  (((1155074 "mast cell activation" ("celf"))
    (3864 "Arthritis" ("dsyn"))
    "AFFECTS"
    "celf"
    "dsyn"
    (76040540))
   ((939537 "Imatinib mesylate" ("orch" "phsu"))
    (3864 "Arthritis" ("dsyn"))
    "TREATS"
    "phsu"
    "dsyn"
    (62390822 47235373)))
  (((1155074 "mast cell activation" ("celf"))
    (8679 "Chronic Disease" ("dsyn"))
    "AFFECTS"
    "celf"
    "dsyn"
    (22892042 22892027))
   ((935989 "imatinib" ("phsu" "orch"))
    (8679 "Chronic Disease" ("dsyn"))
    "TREATS"
    "orch"
    "dsyn"
    (83824572)))
  (((1155074 "mast cell activation" ("celf"))
    (8679 "Chronic Disease" ("dsyn"))
    "AFFECTS"
    "celf"
    "dsyn"
    (22892042 22892027))
   ((935989 "imatinib" ("phsu" "orch"))
    (8679 "Chronic Disease" ("dsyn"))
    "TREATS"
    "phsu"
    "dsyn"
    (80065397 70145883)))
  (((1155074 "mast cell activation" ("celf"))
    (12634 "Disease" ("dsyn"))
    "AFFECTS"
    "celf"
    "dsyn"
    (85156399
     66251757
     65830637
     61542167
     55781135
     55026706
     52273769
     48853539
     41997247
     34204920
     30957315
     30619348
     22892051
     22892032
     22074757))
   ((935989 "imatinib" ("phsu" "orch"))
    (12634 "Disease" ("dsyn"))
    "TREATS"
    "orch"
    "dsyn"
    (86095345 67097950 63793027)))
  (((1155074 "mast cell activation" ("celf"))
    (12634 "Disease" ("dsyn"))
    "AFFECTS"
    "celf"
    "dsyn"
    (85156399
     66251757
     65830637
     61542167
     55781135
     55026706
     52273769
     48853539
     41997247
     34204920
     30957315
     30619348
     22892051
     22892032
     22074757))
   ((935989 "imatinib" ("phsu" "orch"))
    (12634 "Disease" ("dsyn"))
    "TREATS"
    "phsu"
    "dsyn"
    (89407082
     87982863
     84505267
     83839341
     82894881
     79751398
     77831693
     74132108
     73588617
     71258609
     68415786
     67458023
     64623720
     63394606
     63276539
     62770748
     61655728
     61327847
     59631443
     58825872
     57593433
     56055322
     55987577
     55898005
     55505236
     54274523
     54274472
     54245112
     53414031
     53353967
     51836186
     50884827
     50551010
     47162081
     44666378
     44579312
     43522756
     41787008)))
  (((1155074 "mast cell activation" ("celf"))
    (12634 "Disease" ("dsyn"))
    "AFFECTS"
    "celf"
    "dsyn"
    (85156399
     66251757
     65830637
     61542167
     55781135
     55026706
     52273769
     48853539
     41997247
     34204920
     30957315
     30619348
     22892051
     22892032
     22074757))
   ((939537 "Imatinib mesylate" ("orch" "phsu"))
    (12634 "Disease" ("dsyn"))
    "TREATS"
    "orch"
    "dsyn"
    (54632685)))
  (((1155074 "mast cell activation" ("celf"))
    (12634 "Disease" ("dsyn"))
    "AFFECTS"
    "celf"
    "dsyn"
    (85156399
     66251757
     65830637
     61542167
     55781135
     55026706
     52273769
     48853539
     41997247
     34204920
     30957315
     30619348
     22892051
     22892032
     22074757))
   ((939537 "Imatinib mesylate" ("orch" "phsu"))
    (12634 "Disease" ("dsyn"))
    "TREATS"
    "phsu"
    "dsyn"
    (79915447 62155051 53028960 51345693 35868680 35397483)))
  (((1155074 "mast cell activation" ("celf"))
    (12634 "Disease" ("dsyn"))
    "AFFECTS"
    "celf"
    "dsyn"
    (85156399
     66251757
     65830637
     61542167
     55781135
     55026706
     52273769
     48853539
     41997247
     34204920
     30957315
     30619348
     22892051
     22892032
     22074757))
   ((1331284 "imatinib 400 MG" ("clnd"))
    (12634 "Disease" ("dsyn"))
    "TREATS"
    "clnd"
    "dsyn"
    (82894882)))
  (((1155074 "mast cell activation" ("celf"))
    (26769 "Multiple Sclerosis" ("dsyn"))
    "AFFECTS"
    "celf"
    "dsyn"
    (17260890))
   ((935989 "imatinib" ("phsu" "orch"))
    (26769 "Multiple Sclerosis" ("dsyn"))
    "TREATS"
    "phsu"
    "dsyn"
    (84787125 64871514)))
  (((1155074 "mast cell activation" ("celf"))
    (41296 "Tuberculosis" ("dsyn"))
    "AFFECTS"
    "celf"
    "dsyn"
    (62139776))
   ((935989 "imatinib" ("phsu" "orch"))
    (41296 "Tuberculosis" ("dsyn"))
    "TREATS"
    "phsu"
    "dsyn"
    (55204927)))
  (((1155074 "mast cell activation" ("celf"))
    (272203 "Indolent Systemic Mastocytosis" ("dsyn"))
    "AFFECTS"
    "celf"
    "dsyn"
    (75547483))
   ((935989 "imatinib" ("phsu" "orch"))
    (272203 "Indolent Systemic Mastocytosis" ("dsyn"))
    "TREATS"
    "phsu"
    "dsyn"
    (86365747 86365718))))
;;
(time (run* (e-mast/disease e-imatinib/disease)
        (fresh (x d p e-rest)
          (fuzzy-concepto "mast cell activation" x)
          (== `(,x ,d ,p . ,e-rest) e-mast/disease)
          (conde
            [(== "AFFECTS" p)]
            [(== "CAUSES" p)])
          (edgeo e-mast/disease)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) d)
            (membero "dsyn" concept-type*))

          (fresh (s-imatinib/disease m-imatinib/disease p-imatinib/disease e-rest-imatinib/disease)
            (== `(,s-imatinib/disease ,m-imatinib/disease ,p-imatinib/disease . ,e-rest-imatinib/disease) e-imatinib/disease)
            (fuzzy-concepto "imatinib" s-imatinib/disease)
            (== m-imatinib/disease d)
            (== "TREATS" p-imatinib/disease)
            (edgeo e-imatinib/disease))          
          )))

;; diseases directly caused by or affected by mast cell activation (18 of them)
'(((1155074 "mast cell activation" ("celf"))
   (3864 "Arthritis" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (76040540))
  ((1155074 "mast cell activation" ("celf"))
   (4096 "Asthma" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (54247735 38643255))
  ((1155074 "mast cell activation" ("celf"))
   (8679 "Chronic Disease" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (22892042 22892027))
  ((1155074 "mast cell activation" ("celf"))
   (9766 "Allergic Conjunctivitis" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (59114948))
  ((1155074 "mast cell activation" ("celf"))
   (12634 "Disease" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (85156399
    66251757
    65830637
    61542167
    55781135
    55026706
    52273769
    48853539
    41997247
    34204920
    30957315
    30619348
    22892051
    22892032
    22074757))
  ((1155074 "mast cell activation" ("celf"))
   (14038 "Encephalitis" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (65637409))
  ((1155074 "mast cell activation" ("celf"))
   (26769 "Multiple Sclerosis" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (17260890))
  ((1155074 "mast cell activation" ("celf"))
   (38644 "Sudden infant death syndrome" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (26368942))
  ((1155074 "mast cell activation" ("celf"))
   (41296 "Tuberculosis" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (62139776))
  ((1155074 "mast cell activation" ("celf"))
   (155877 "Extrinsic asthma NOS" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (83500602))
  ((1155074 "mast cell activation" ("celf"))
   (263338 "Chronic urticaria" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (62388241))
  ((1155074 "mast cell activation" ("celf"))
   (272203 "Indolent Systemic Mastocytosis" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (75547483))
  ((1155074 "mast cell activation" ("celf"))
   (282488 "Interstitial Cystitis" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (23789469))
  ((1155074 "mast cell activation" ("celf"))
   (340865 "Anaphylactoid reaction" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (43841329))
  ((1155074 "mast cell activation" ("celf"))
   (853897 "Diabetic cardiomyopathy" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (72760736))
  ((1155074 "mast cell activation" ("celf"))
   (948089 "Acute coronary syndrome" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (85902244
    70809425
    63933449
    62132544
    61479825
    58213540
    58052580
    56749622
    48540985
    43841336))
  ((1155074 "mast cell activation" ("celf"))
   (1290886 "Chronic inflammatory disorder" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (58131314))
  ((1155074 "mast cell activation" ("celf"))
   (1449852 "Erythematotelangiectatic Rosacea" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (71577548)))
;;
#|
(time (run* (e)
        (fresh (x d p e-rest)
          (fuzzy-concepto "mast cell activation" x)
          (== `(,x ,d ,p . ,e-rest) e)
          (conde
            [(== "AFFECTS" p)]
            [(== "CAUSES" p)])
          (edgeo e)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) d)
            (membero "dsyn" concept-type*))
          )))
|#

;; cpu time: 132126 real time: 132712 gc time: 6589
;; 20000 paths
#|
(time (length (run 20000 (path)
                (fresh (x)
                  (fuzzy-concepto "mast cell activation" x)
                  (path-to-diseaseo x path)))))
|#

;; time to generate all genes that are inhibited by imatinib that in turn cause some disease, and where imatinib is known to directly treat that disease
;; cpu time: 88571 real time: 89553 gc time: 1150
;; 9681

(time (pretty-print (run* (tree)
                      (fresh (e-imatinib/known-disease s-imatinib/known-disease m-imatinib/known-disease p-imatinib/known-disease e-rest-imatinib/known-disease
                              e-imatinib/gene s-imatinib/gene m-imatinib/gene p-imatinib/gene e-rest-imatinib/gene
                              e-gene/known-disease s-gene/known-disease m-gene/known-disease p-gene/known-disease e-rest-gene/known-disease
                              )

                        (== `(,s-imatinib/known-disease ,m-imatinib/known-disease ,p-imatinib/known-disease . ,e-rest-imatinib/known-disease) e-imatinib/known-disease)
                        (== `(,s-imatinib/gene ,m-imatinib/gene ,p-imatinib/gene . ,e-rest-imatinib/gene) e-imatinib/gene)
                        (== `(,s-gene/known-disease ,m-gene/known-disease ,p-gene/known-disease . ,e-rest-gene/known-disease) e-gene/known-disease)

                        ;; need to add types!  https://mmtx.nlm.nih.gov/MMTx/semanticTypes.shtml
                        ;; (map (lambda (cui) (hash-ref cui=>concept cui)) (hash-ref semtype-id=>cui* (hash-ref semtype=>id "gngm")))
                        ;; * gene:     gngm	T028	Gene or Genome       (hash-ref semtype=>id "gngm") => 59      (hash-ref semtype-id=>cui* (hash-ref semtype=>id "gngm"))
                        ;; * known disease
                        ;; * unknown disease
                        
                        (fuzzy-concepto "imatinib" s-imatinib/known-disease)

                        ;; same imatinib
                        (== s-imatinib/known-disease s-imatinib/gene)

                        ;; same gene
                        (== s-gene/known-disease m-imatinib/gene)

                        ;; same known disease
                        (== m-imatinib/known-disease m-gene/known-disease)
                        
                        (== "TREATS" p-imatinib/known-disease)

                        (== "CAUSES" p-gene/known-disease)
                        
                        (== "INHIBITS" p-imatinib/gene)
                        
                        (edgeo e-imatinib/gene)

                        ;; filter to make sure 'gene' is actually a gene!
                        ;; what is a cleaner way to do this?
                        (fresh (cui name concept-type*)
                          (== `(,cui ,name ,concept-type*) m-imatinib/gene)
                          (membero "gngm" concept-type*))
                        
                        (edgeo e-gene/known-disease)
                        (edgeo e-imatinib/known-disease)

                        (== `(,e-imatinib/gene ,e-gene/known-disease ,e-imatinib/known-disease) tree)))))


;; no results!  good!  what we would hope for
#|
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (fuzzy-concepto "imatinib" s)
                        (fuzzy-concepto "asthma" m)
                        (edgeo e)))))
|#

;; no results!  good!  what we would hope for
#|
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (fuzzy-concepto "imatinib" s)
                        (fuzzy-concepto "mast cell activation" m)
                        (edgeo e)))))
|#

'(((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (1155074 "mast cell activation" ("celf"))
   "AFFECTS"
   "gngm"
   "celf"
   (45683353))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (1155074 "mast cell activation" ("celf"))
   "CAUSES"
   "gngm"
   "celf"
   (36804978)))
#|
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (fuzzy-concepto "KIT gene" s)
                        (fuzzy-concepto "mast cell activation" m)
                        (edgeo e)))))
|#

'((4096 "Asthma" ("dsyn"))
  (4099 "Asthma, Exercise-Induced" ("dsyn"))
  (14434 "Detergent asthma" ("dsyn"))
  (38218 "Status Asthmaticus" ("dsyn"))
  (155877 "Extrinsic asthma NOS" ("dsyn"))
  (155880 "Intrinsic asthma NOS" ("dsyn"))
  (238266 "Meat-wrappers' asthma" ("dsyn"))
  (238375 "Platinum asthma" ("dsyn"))
  (259745 "Asthma, infective" ("dsyn"))
  (259808 "Asthma, endogenous" ("dsyn"))
  (264348 "Chronic asthmatic bronchitis" ("dsyn"))
  (264408 "Childhood asthma" ("dsyn"))
  (264411 "Hay fever with asthma" ("dsyn"))
  (264413 "Late onset asthma" ("dsyn"))
  (264423 "Occupational asthma" ("dsyn"))
  (264480 "Bakers' asthma" ("dsyn"))
  (282556 "Anti-Asthmatic Agents" ("phsu"))
  (340067 "Drug-induced asthma" ("dsyn"))
  (340069 "Colophony asthma" ("dsyn"))
  (340070 "Millers' asthma" ("dsyn"))
  (340073 "Factitious asthma" ("dsyn"))
  (340076 "Asthmatic pulmonary eosinophilia" ("dsyn"))
  (340094 "Wood asthma" ("dsyn"))
  (347950 "Asthma attack NOS" ("dsyn"))
  (348819 "Mixed asthma" ("dsyn"))
  (349790 "Exacerbation of asthma" ("fndg"))
  (350348 "Asthma prophylaxis" ("phsu"))
  (392681 "Asthmatic breathing" ("sosy"))
  (420048 "Asthma screening" ("hlca"))
  (420293 "Emergency admission, asthma" ("hlca"))
  (543699 "ASA intolerant asthma" ("dsyn"))
  (554832 "Asthma monitoring" ("hlca"))
  (581122 "Asthma severity" ("hlca"))
  (581124 "Mild asthma" ("fndg"))
  (581125 "Moderate asthma" ("fndg"))
  (581126 "Severe asthma" ("fndg"))
  (582415 "Acute asthma" ("dsyn"))
  (606809 "Asthma 23D" ("phsu"))
  (684913 "Chemical-induced asthma" ("dsyn"))
  (729337 "Brittle asthma" ("dsyn"))
  (741266 "ASTHMA STABLE" ("fndg"))
  (856716 "Asthma aspirin-sensitive" ("dsyn"))
  (859987 "Asthmatoid bronchitis" ("dsyn"))
  (876293 "Asthma Monitoring System" ("medd"))
  (877264 "Infantile asthma" ("dsyn"))
  (877430 "Asthma chronic" ("dsyn"))
  (1135801 "Tylophora asthmatica" ("plnt"))
  (1261327 "Family history of asthma" ("fndg"))
  (1271086 "Suspected asthma" ("fndg"))
  (1272273 "Asthma finding" ("fndg"))
  (1303029 "Asthma trigger" ("clna"))
  (1318955 "Asthma management" ("hlca"))
  (1319018 "Asthmatic bronchitis" ("dsyn"))
  (1319853 "Aspirin-induced asthma" ("fndg"))
  (1328364 "Analgesic asthma syndrome" ("inpo")))
#|
(run* (m) (fuzzy-concepto "asthma" m))
|#

'(((1155074 "mast cell activation" ("celf"))
   (4096 "Asthma" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (54247735 38643255))
  ((1155074 "mast cell activation" ("celf"))
   (4096 "Asthma" ("dsyn"))
   "ASSOCIATED_WITH"
   "celf"
   "dsyn"
   (13130971))
  ((1155074 "mast cell activation" ("celf"))
   (4099 "Asthma, Exercise-Induced" ("dsyn"))
   "NEG_AFFECTS"
   "celf"
   "dsyn"
   (17055287))
  ((1155074 "mast cell activation" ("celf"))
   (155877 "Extrinsic asthma NOS" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (83500602)))
#|
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (fuzzy-concepto "mast cell activation" s)
                        (fuzzy-concepto "asthma" m)
                        (edgeo e)))))
|#

'("AFFECTS" "ASSOCIATED_WITH" "NEG_AFFECTS" "AFFECTS")
#|
(time (pretty-print (run* (p)
                      (fresh (e s m e-rest)
                        (== `(,s ,m ,p . ,e-rest) e)
                        (fuzzy-concepto "mast cell activation" s)
                        (fuzzy-concepto "asthma" m)
                        (edgeo e)))))
|#


'("AFFECTS" "ASSOCIATED_WITH")
#|
(time (pretty-print (run* (p)
                      (fresh (e s m e-rest)
                        (== `(,s ,m ,p . ,e-rest) e)
                        (cuio s 1155074) ; (1155074 "mast cell activation" ("celf"))
                        (cuio m 4096)    ; (4096 "Asthma" ("dsyn"))
                        (edgeo e)))))
|#


'("AFFECTS"
  "AFFECTS"
  "ASSOCIATED_WITH"
  "ASSOCIATED_WITH"
  "ASSOCIATED_WITH"
  "ASSOCIATED_WITH"
  "AUGMENTS"
  "CAUSES"
  "AUGMENTS"
  "NEG_ASSOCIATED_WITH"
  "CAUSES"
  "PART_OF"
  "CAUSES"
  "PREDISPOSES"
  "DISRUPTS"
  "TREATS"
  "NEG_ASSOCIATED_WITH"
  "NEG_PART_OF"
  "NEG_TREATS"
  "PART_OF"
  "PREDISPOSES"
  "PREDISPOSES"
  "PREVENTS"
  "TREATS"
  "compared_with"
  "higher_than")
#|
(time (pretty-print (run* (p)
                      (fresh (e s m e-rest)
                        (== `(,s ,m ,p . ,e-rest) e)
                        (conde
                          [(cuio s 1416655)] ; (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
                          [(cuio s 920288)]) ; (920288 "C-KIT Gene" ("gngm" "aapp"))
                        (cuio m 238198)      ; (238198 "Gastrointestinal Stromal Tumors" ("neop"))
                        (edgeo e)))))
|#




'(((935989 "imatinib" ("phsu" "orch"))
   (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   "INHIBITS"
   "orch"
   "gngm"
   (88094027
    82038640
    78690628
    78513788
    70397515
    60608992
    57775955
    56779144
    55866397
    55866394
    54750176
    54602555
    54524739
    53954674
    53827456
    53794226
    53155624
    51843305
    51685933
    50494576
    50287491
    50287227
    49443008
    48324562
    47259531
    45719202
    44323647
    44187569
    43969275
    40811261
    40130263
    35363677
    35363677))
  ((939537 "Imatinib mesylate" ("orch" "phsu"))
   (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   "INHIBITS"
   "orch"
   "gngm"
   (78245416
    60923184
    52405281
    50198713
    48733408
    48658568
    46535660
    45216976
    44323573
    42932218
    41594062
    41594059
    41109626
    41017098
    40494392
    37687910))
  ((935989 "imatinib" ("phsu" "orch"))
   (920288 "C-KIT Gene" ("gngm" "aapp"))
   "INHIBITS"
   "orch"
   "gngm"
   (88061703
    85297819
    84466544
    80622734
    80622529
    80622496
    80602335
    80602240
    74425222
    73925398
    72612280
    70407634
    68644392
    68644322
    61811120
    57954791
    57560853
    57461754
    56194601
    56139137
    55779007
    54837106
    53185505
    53155624
    52291848
    51896005
    49170588
    46720307
    45481706
    42521897
    41357396
    40935122))
  ((939537 "Imatinib mesylate" ("orch" "phsu"))
   (920288 "C-KIT Gene" ("gngm" "aapp"))
   "INHIBITS"
   "orch"
   "gngm"
   (82419991
    73925304
    61340408
    56865479
    55254090
    54481089
    50302706
    47020133
    43901262
    42424750
    41410459
    41146783
    39633613)))
#|
(time (pretty-print (run* (e)
                      (fresh (s m p e-rest)
                        (== `(,s ,m ,p . ,e-rest) e)
                        (conde
                          [(cuio s 935989)]  ; (935989 "imatinib" ("phsu" "orch"))
                          [(cuio s 939537)]) ; (939537 "Imatinib mesylate" ("orch" "phsu"))
                        (conde
                          [(cuio m 1416655)]  ; (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
                          [(cuio m 920288)]) ; (920288 "C-KIT Gene" ("gngm" "aapp"))
                        (== "INHIBITS" p)
                        (edgeo e)))))
|#

'("COEXISTS_WITH"
  "INHIBITS"
  "COEXISTS_WITH"
  "INTERACTS_WITH"
  "INTERACTS_WITH"
  "INHIBITS"
  "NEG_INTERACTS_WITH"
  "STIMULATES"
  "COEXISTS_WITH"
  "INTERACTS_WITH"
  "compared_with"
  "COEXISTS_WITH"
  "lower_than"
  "NEG_INHIBITS"
  "INHIBITS"
  "STIMULATES"
  "INHIBITS"
  "USES"
  "INTERACTS_WITH"
  "INTERACTS_WITH"
  "INTERACTS_WITH"
  "INTERACTS_WITH"
  "NEG_INTERACTS_WITH")
#|
(time (pretty-print (run* (p)
                      (fresh (e s m e-rest)
                        (== `(,s ,m ,p . ,e-rest) e)
                        (conde
                          [(cuio s 935989)]  ; (935989 "imatinib" ("phsu" "orch"))
                          [(cuio s 939537)]) ; (939537 "Imatinib mesylate" ("orch" "phsu"))
                        (conde
                          [(cuio m 1416655)]  ; (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
                          [(cuio m 920288)]) ; (920288 "C-KIT Gene" ("gngm" "aapp"))
                        (edgeo e)))))
|#


;; no answers!  good!  This means we will need to find the connection ourselves, assuming it exists in SemMedDB
#|
(time (pretty-print (run* (p)
                      (fresh (e s m e-rest)
                        (== `(,s ,m ,p . ,e-rest) e)
                        (conde
                          [(cuio s 935989)]  ; (935989 "imatinib" ("phsu" "orch"))
                          [(cuio s 939537)]) ; (939537 "Imatinib mesylate" ("orch" "phsu"))
                        (fuzzy-concepto "asthma" m)
                        (edgeo e)))))
|#


;; imatinib
;; "AFFECTS"
;; "AUGMENTS"
;; "CAUSES"
;; "NEG_AFFECTS"
;; "NEG_TREATS"
;; "PREDISPOSES"
;; "PREVENTS"
;; "TREATS"
;; Gastrointestinal Stromal Tumors
#|
(time (pretty-print (run* (p)
                      (fresh (e s m e-rest)
                        (== `(,s ,m ,p . ,e-rest) e)
                        (conde
                          [(cuio s 935989)]  ; (935989 "imatinib" ("phsu" "orch"))
                          [(cuio s 939537)]) ; (939537 "Imatinib mesylate" ("orch" "phsu"))
                        (cuio m 238198)      ; (238198 "Gastrointestinal Stromal Tumors" ("neop"))
                        (edgeo e)))))
|#

;; What else, other than "KIT gene", causes "Gastrointestinal Stromal Tumors"?
;;
;; 46 entries in the list
;;
;; hmmm...
;;
;; ((935989 "imatinib" ("phsu" "orch"))
;;  (238198 "Gastrointestinal Stromal Tumors" ("neop"))
;;  "CAUSES"
;;  "orch"
;;  "neop"
;;  (76776830 56175577 55046179))
;; ((939537 "Imatinib mesylate" ("orch" "phsu"))
;;  (238198 "Gastrointestinal Stromal Tumors" ("neop"))
;;  "CAUSES"
;;  "orch"
;;  "neop"
;;  (81096076 44044998))
;;
;;
;; Genes
;; Proto-Oncogenes
;;
;; CCND3
;; FRAP1
;; KIT
;; PDGFRA
;; SARDH
;; SDHB
;; SDS
;; VEGFA
;;
;; Oncogene Proteins
;;
;; Proto-Oncogene Protein c-kit
;; FRAP1 protein, human
;; PDGFA protein, human
;;
;; Receptor Protein-Tyrosine Kinases
;; Mitogen-Activated Protein Kinases
;;
;; Protein-tyrosine kinase inhibitor
;;
'(((6674 "Calcitriol" ("horm" "strd" "phsu" "vita"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "horm"
   "neop"
   (77145307))
  ((7090 "Carcinogens" ("hops"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "hops"
   "neop"
   (65449228 50883408))
  ((7621 "Cell Transformation, Neoplastic" ("neop"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "neop"
   "neop"
   (63043249))
  ((13299 "Duodenogastric Reflux" ("dsyn"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "dsyn"
   "neop"
   (42175197))
  ((17337 "Genes" ("aapp" "gngm"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "gngm"
   "neop"
   (58489737))
  ((24002 "Lorazepam" ("phsu" "orch"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "orch"
   "neop"
   (48186889))
  ((26336 "Study models" ("inpr" "resd"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "resd"
   "neop"
   (45629888))
  ((27627 "Neoplasm Metastasis" ("neop"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "neop"
   "neop"
   (63071652))
  ((29005 "Oncogene Proteins" ("gngm" "bacs" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (46756812))
  ((31727 "Phosphotransferases" ("aapp" "gngm" "enzy"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (65449233))
  ((32200 "Platelet-Derived Growth Factor" ("gngm" "aapp" "bacs"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "gngm"
   "neop"
   (85323763 85323762))
  ((33713 "Proto-Oncogenes" ("aapp" "gngm"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "gngm"
   "neop"
   (62178248 59318007 39441233))
  ((36442 "Scopolamine" ("orch" "phsu"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "orch"
   "neop"
   (48186893))
  ((37659 "Somatostatin" ("phsu" "aapp" "gngm" "horm"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (61820493))
  ((38615 "Succinate Dehydrogenase" ("aapp" "enzy" "gngm"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (85537008 55521924 42919388))
  ((40646 "Transcriptase" ("gngm" "aapp" "enzy"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (76380814))
  ((55817 "citrate carrier" ("bacs" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (57723826))
  ((71253 "Platelet-Derived Growth Factor Receptor" ("aapp" "gngm" "enzy"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "gngm"
   "neop"
   (53789101 53789073 50274414))
  ((72470 "Proto-Oncogene Protein c-kit" ("aapp" "gngm" "rcpt" "imft"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (59480455 47920692))
  ((206364 "Receptor Protein-Tyrosine Kinases" ("enzy" "rcpt" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (86726299 50789329 29550497))
  ((206530 "Germ-Line Mutation" ("genf"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "genf"
   "neop"
   (73582365))
  ((243077 "inhibitors" ("chvf"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "chvf"
   "neop"
   (70292056))
  ((244104 "Pyruvate" ("orch" "bacs"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "bacs"
   "neop"
   (75527580))
  ((290067
    "Platelet-Derived Growth Factor alpha Receptor"
    ("rcpt" "aapp" "gngm" "enzy"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (43196739 40249105))
  ((450442 "Agent" ("chvf"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "chvf"
   "neop"
   (68259613))
  ((534628 "Endostatins" ("aapp" "phsu" "gngm"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (50654712))
  ((725066 "Advance" ("medd"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "medd"
   "neop"
   (59465228))
  ((752312 "Mitogen-Activated Protein Kinases" ("enzy" "aapp" "gngm"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (54020272))
  ((920288 "C-KIT Gene" ("gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "gngm"
   "neop"
   (87384637 51889889 37195338 33819150 33819075))
  ((935989 "imatinib" ("phsu" "orch"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "orch"
   "neop"
   (76776830 56175577 55046179))
  ((939537 "Imatinib mesylate" ("orch" "phsu"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "orch"
   "neop"
   (81096076 44044998))
  ((1268567 "Protein-tyrosine kinase inhibitor" ("phsu"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "phsu"
   "neop"
   (63024462))
  ((1307407 "FRAP1 protein, human" ("aapp" "enzy" "gngm"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (54020280))
  ((1333132 "Common Neoplasm" ("neop"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "neop"
   "neop"
   (43813786))
  ((1335200 "PDGFA gene" ("gngm" "horm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (59625566))
  ((1335201 "PDGFRA gene" ("rcpt" "enzy" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (43196739 40249105))
  ((1335201 "PDGFRA gene" ("rcpt" "enzy" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "gngm"
   "neop"
   (87264286
    87264286
    86468474
    86468474
    83221033
    83221033
    77629821
    77629821
    61335412
    61335412
    60497536
    60497536
    48640418
    48640418
    42671657
    42671657
    41043821
    41043821
    41043806
    41043806))
  ((1335430 "PDGFA protein, human" ("horm" "aapp" "gngm"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (59625566))
  ((1413175 "CCND3 gene" ("aapp" "gngm"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (41147975 41147975))
  ((1414805 "FRAP1 gene" ("aapp" "gngm" "enzy" "bacs" "aapp" "gngm"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (54020280))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (59625558 59480455 43196727 40249098))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "gngm"
   "neop"
   (87384637
    85323758
    85323753
    83221026
    61335391
    58431044
    57775929
    55436679
    54253302
    53789085
    53789078
    52927884
    50274407
    50049773
    49635451
    46841526
    46757085
    46299485
    44187839
    42671642
    41287669
    37195338
    34247527
    33819150
    33819075
    33114772
    32425400
    31543639
    31187106
    27728484
    27728480))
  ((1419817 "SARDH gene" ("enzy" "gngm" "bacs" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "gngm"
   "neop"
   (66904284))
  ((1419907 "SDHB gene" ("aapp" "gngm" "enzy" "bacs"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "gngm"
   "neop"
   (66904284))
  ((1419917 "SDS gene" ("gngm" "enzy" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "gngm"
   "neop"
   (66904284))
  ((1823619 "VEGFA gene" ("bacs" "phsu" "rcpt" "gngm" "imft" "enzy" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (50654702)))
;;
#|
(time (pretty-print (run* (e)
                      (fresh (s m p e-rest)
                        (== `(,s ,m ,p . ,e-rest) e)
                        (cuio m 238198)
                        (== "CAUSES" p)
                        (edgeo e)))))
|#

;; "KIT gene"                        CUI 1416655
;; "Gastrointestinal Stromal Tumors" CUI  238198
;; "CAUSES"
;;
'(((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (59625558 59480455 43196727 40249098))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "gngm"
   "neop"
   (87384637
    85323758
    85323753
    83221026
    61335391
    58431044
    57775929
    55436679
    54253302
    53789085
    53789078
    52927884
    50274407
    50049773
    49635451
    46841526
    46757085
    46299485
    44187839
    42671642
    41287669
    37195338
    34247527
    33819150
    33819075
    33114772
    32425400
    31543639
    31187106
    27728484
    27728480)))
;;
;; Sometimes we just want to target a specific cui or set of cuis.
;;
;; #(1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
;; #(238198 "Gastrointestinal Stromal Tumors" ("neop"))
;;
#|
(time (pretty-print (run* (e)
                      (fresh (s m p e-rest)
                        (== `(,s ,m ,p . ,e-rest) e)
                        (cuio s 1416655)
                        (cuio m 238198)
                        (== "CAUSES" p)
                        (edgeo e)))))
|#


;; "KIT gene"
;; "Gastrointestinal Stromal Tumors"
;; (any predicate, using CUIs for S and M)
'(((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "AFFECTS"
   "gngm"
   "neop"
   (76251827 71712139 66437646 51229875 45105359 44957091))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "ASSOCIATED_WITH"
   "aapp"
   "neop"
   (88816704
    84460940
    82018014
    80113913
    78220375
    68676785
    64194050
    62488657
    62488417
    60516197
    59023488
    57460066
    55504934
    51048056
    50221433
    49515351
    42281387
    39806474
    39806458
    38471241
    36205524
    32077844
    27305882))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "ASSOCIATED_WITH"
   "gngm"
   "neop"
   (88816957
    88816951
    88816950
    88786674
    88707141
    87354026
    87354024
    86927567
    85526693
    85323889
    85258402
    85131831
    84724778
    84460748
    84460694
    84031830
    84026339
    83653907
    83346329
    83173556
    83173380
    82814810
    82814708
    82174828
    80961594
    80611295
    78681164
    77814508
    77327848
    76776638
    76419851
    76251730
    75963048
    75962942
    75527334
    75208407
    74994400
    74617283
    74420763
    74324760
    73846506
    73776862
    73338226
    73151949
    72728471
    72728225
    71621316
    71588638
    71193030
    71047813
    70628389
    70628117
    70576282
    70576118
    70575948
    69690124
    69539621
    69450585
    69450375
    69393867
    69063607
    68636078
    68635898
    67326540
    66443153
    66443088
    66245060
    66000227
    65490262
    65393355
    65260310
    64512628
    64194332
    64038744
    63974605
    63909178
    63900746
    63874949
    63557002
    63556951
    63247245
    63247245
    62577550
    62470425
    62318601
    62318554
    62076711
    62065944
    61963535
    61807548
    61800883
    61785532
    61273686
    61273686
    60839259
    60522504
    60449191
    59983733
    59983622
    59867401
    59853109
    59625513
    59092968
    59092805
    59092335
    59023393
    58702914
    58430466
    58195432
    58194874
    58155550
    58155464
    58155283
    58155041
    58038459
    56779325
    56779149
    56778962
    56736280
    56175619
    55308106
    55122597
    54971952
    54602893
    54602798
    54602493
    54348559
    54348197
    54348120
    54086008
    54019767
    53993460
    53993388
    53838851
    53617803
    53455495
    53161441
    53155629
    53124815
    52370481
    52188869
    51979898
    51938438
    51764519
    51764101
    51764046
    51650446
    51650436
    50862437
    50658113
    50427094
    50221579
    50122432
    49416815
    49178731
    49156492
    48740872
    48230769
    48230016
    48229895
    48229849
    48061753
    47300231
    47273474
    47179155
    47085455
    46756696
    46673762
    46054806
    45858226
    45698328
    45698328
    45431595
    45253719
    45216614
    45106015
    45105926
    45105310
    44823438
    44574166
    44574034
    44464738
    44395150
    44383506
    44382955
    44382901
    44153320
    43711388
    43604490
    43229970
    42932350
    42860155
    42860155
    42224429
    42190272
    42190171
    41720202
    41253388
    41053204
    41016754
    41016696
    40493615
    40429006
    40428933
    40249147
    40248995
    40247013
    39580372
    39482360
    39185794
    37687613
    37195326
    37195320
    36962598
    36788987
    36751918
    36434304
    36204927
    35865596
    35667659
    35667587
    35363207
    35363207
    34290017
    34246396
    34246279
    33114693
    33114682
    32970840
    32815113
    32656219
    32655995
    32424902
    31894299
    31275672
    29441440
    29440617
    28577961
    28372002
    28371887
    28181385
    28115930
    27578892
    27427757
    26885688
    26862862
    26407311))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "AUGMENTS"
   "aapp"
   "neop"
   (48230271))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "AUGMENTS"
   "gngm"
   "neop"
   (50049662))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (59625558 59480455 43196727 40249098))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "gngm"
   "neop"
   (87384637
    85323758
    85323753
    83221026
    61335391
    58431044
    57775929
    55436679
    54253302
    53789085
    53789078
    52927884
    50274407
    50049773
    49635451
    46841526
    46757085
    46299485
    44187839
    42671642
    41287669
    37195338
    34247527
    33819150
    33819075
    33114772
    32425400
    31543639
    31187106
    27728484
    27728480))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "DISRUPTS"
   "gngm"
   "neop"
   (44804518))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "NEG_ASSOCIATED_WITH"
   "gngm"
   "neop"
   (58372927 42789720))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "NEG_PART_OF"
   "gngm"
   "neop"
   (42672081))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "NEG_TREATS"
   "aapp"
   "neop"
   (53583614))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "PART_OF"
   "gngm"
   "neop"
   (88816701
    88504351
    87664119
    86527022
    86527021
    85431031
    84724497
    82274147
    81758346
    81503677
    81503592
    80572441
    80301506
    80113871
    79973963
    79689424
    78726567
    77821336
    77196050
    77196050
    76776826
    76776713
    76548783
    76252420
    75385720
    75385282
    75385190
    74686997
    74324816
    73776735
    73151923
    72884934
    71444438
    70575975
    70335620
    70335615
    69854253
    69854002
    68641343
    67432607
    67356874
    67246601
    67092734
    67092730
    66938421
    66938290
    64461732
    64424903
    63702674
    63702626
    63702530
    63556857
    63556721
    63556717
    63491002
    61959886
    61806767
    61806198
    60522410
    60096404
    60096404
    58546477
    58546256
    58155422
    58155406
    57723933
    56641477
    56216003
    55455524
    55122648
    55122468
    54602884
    54602791
    54281565
    53839416
    52398498
    52189617
    52064489
    52064364
    51366471
    51366401
    51232846
    50359952
    50359900
    49606405
    49527188
    49527185
    49527095
    48065218
    48065150
    48061838
    47942137
    46383271
    46115797
    45811639
    45811639
    45256545
    45105920
    45105498
    45105492
    45097136
    44823923
    44357089
    44356901
    43533383
    43229922
    43066454
    42789714
    42628108
    42119394
    41205270
    40864010
    40674289
    40347845
    40036044
    38954984
    38954930
    38398428
    36262989
    36202499
    35397427
    35363368
    34259788
    31893874
    28372275
    28372177
    28372114
    28372013
    28372007
    28371822))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "PREDISPOSES"
   "aapp"
   "neop"
   (83625667 76441483 64529929 28452099))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "PREDISPOSES"
   "gngm"
   "neop"
   (82889672 76270951 70297541 68769591 67716993 53756432 51412256 27939940))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "PREVENTS"
   "aapp"
   "neop"
   (54419758))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "TREATS"
   "aapp"
   "neop"
   (89381133
    87264300
    84046111
    83347331
    83212313
    83212026
    78726651
    76441724
    73846868
    73616831
    72119603
    71290296
    63973922
    63557099
    62693228
    61564880
    58650608
    58510784
    58431141
    58431135
    57460563
    56215499
    54580204
    53770840
    51513499
    51414870
    50287763
    48710463
    48636064
    45867849
    45629899
    42593053
    40213719
    40162438
    38953465
    38953454
    36434207
    34247121
    32462103
    32462099
    31121291
    27939123))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "compared_with"
   "gngm"
   "neop"
   (42860729))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "higher_than"
   "gngm"
   "neop"
   (42860733)))
;;
;; Sometimes we just want to target a specific cui or set of cuis.
;;
;; #(1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
;; #(238198 "Gastrointestinal Stromal Tumors" ("neop"))
;;
#|
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (cuio s 1416655)
                        (cuio m 238198)
                        (edgeo e)))))
|#


; (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
;
; directly connected to
;
; (238198 "Gastrointestinal Stromal Tumors" ("neop"))
;
; results:
;
;; predicates between KIT and GIST
;
;; AFFECTS
;; ASSOCIATED_WITH
;; AUGMENTS
;; CAUSES
;; compared_with
;; DISRUPTS
;; higher_than
;; NEG_ASSOCIATED_WITH
;; NEG_PART_OF
;; NEG_TREATS
;; PREDISPOSES
;; PREVENTS
;; TREATS
;
; also, C-KIT and KIT seem redundanct
;
'(((920288 "C-KIT Gene" ("gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "AFFECTS"
   "gngm"
   "neop"
   (46760540))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "AFFECTS"
   "gngm"
   "neop"
   (76251827 71712139 66437646 51229875 45105359 44957091))
  ((920288 "C-KIT Gene" ("gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "ASSOCIATED_WITH"
   "aapp"
   "neop"
   (49526961))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "ASSOCIATED_WITH"
   "aapp"
   "neop"
   (88816704
    84460940
    82018014
    80113913
    78220375
    68676785
    64194050
    62488657
    62488417
    60516197
    59023488
    57460066
    55504934
    51048056
    50221433
    49515351
    42281387
    39806474
    39806458
    38471241
    36205524
    32077844
    27305882))
  ((920288 "C-KIT Gene" ("gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "ASSOCIATED_WITH"
   "gngm"
   "neop"
   (85460993
    84198758
    83346329
    82814810
    82814708
    76419851
    76368784
    75672079
    74687151
    74617283
    74510694
    74420763
    73304122
    70922298
    70922287
    70480510
    70480448
    68259646
    63841800
    63557002
    63556951
    62624485
    62378661
    60839259
    60370637
    59983733
    59092805
    59092335
    58702780
    58430466
    57840570
    56779149
    56778962
    56627960
    56175619
    56140915
    55608208
    54980002
    54971952
    53617803
    53603127
    53595646
    53496246
    53496201
    53155629
    51764519
    51764101
    51764046
    51690076
    50869476
    50427094
    50128173
    50128133
    49526823
    49471986
    49416815
    49388946
    48018585
    46774667
    46760302
    46645481
    46459742
    46318460
    46188441
    46188438
    46036844
    45813589
    45811969
    45408733
    43977736
    43762035
    43384567
    43257978
    43257961
    42977994
    42235291
    42190272
    42190171
    41774698
    41435074
    41345067
    41204705
    40493615
    40429006
    40428933
    39750509
    39580372
    38960183
    38011762
    38011758
    37544890
    37544516
    37195326
    37195320
    37116085
    36434304
    35667587
    35247619
    34156014
    33679770
    32970840
    32815393
    32656219
    31172044
    30747520
    30062221
    29441443
    29440617
    28877968
    28877765
    28877668
    28455552
    27839749
    27720966
    27427757
    26918073
    26738840))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "ASSOCIATED_WITH"
   "gngm"
   "neop"
   (88816957
    88816951
    88816950
    88786674
    88707141
    87354026
    87354024
    86927567
    85526693
    85323889
    85258402
    85131831
    84724778
    84460748
    84460694
    84031830
    84026339
    83653907
    83346329
    83173556
    83173380
    82814810
    82814708
    82174828
    80961594
    80611295
    78681164
    77814508
    77327848
    76776638
    76419851
    76251730
    75963048
    75962942
    75527334
    75208407
    74994400
    74617283
    74420763
    74324760
    73846506
    73776862
    73338226
    73151949
    72728471
    72728225
    71621316
    71588638
    71193030
    71047813
    70628389
    70628117
    70576282
    70576118
    70575948
    69690124
    69539621
    69450585
    69450375
    69393867
    69063607
    68636078
    68635898
    67326540
    66443153
    66443088
    66245060
    66000227
    65490262
    65393355
    65260310
    64512628
    64194332
    64038744
    63974605
    63909178
    63900746
    63874949
    63557002
    63556951
    63247245
    63247245
    62577550
    62470425
    62318601
    62318554
    62076711
    62065944
    61963535
    61807548
    61800883
    61785532
    61273686
    61273686
    60839259
    60522504
    60449191
    59983733
    59983622
    59867401
    59853109
    59625513
    59092968
    59092805
    59092335
    59023393
    58702914
    58430466
    58195432
    58194874
    58155550
    58155464
    58155283
    58155041
    58038459
    56779325
    56779149
    56778962
    56736280
    56175619
    55308106
    55122597
    54971952
    54602893
    54602798
    54602493
    54348559
    54348197
    54348120
    54086008
    54019767
    53993460
    53993388
    53838851
    53617803
    53455495
    53161441
    53155629
    53124815
    52370481
    52188869
    51979898
    51938438
    51764519
    51764101
    51764046
    51650446
    51650436
    50862437
    50658113
    50427094
    50221579
    50122432
    49416815
    49178731
    49156492
    48740872
    48230769
    48230016
    48229895
    48229849
    48061753
    47300231
    47273474
    47179155
    47085455
    46756696
    46673762
    46054806
    45858226
    45698328
    45698328
    45431595
    45253719
    45216614
    45106015
    45105926
    45105310
    44823438
    44574166
    44574034
    44464738
    44395150
    44383506
    44382955
    44382901
    44153320
    43711388
    43604490
    43229970
    42932350
    42860155
    42860155
    42224429
    42190272
    42190171
    41720202
    41253388
    41053204
    41016754
    41016696
    40493615
    40429006
    40428933
    40249147
    40248995
    40247013
    39580372
    39482360
    39185794
    37687613
    37195326
    37195320
    36962598
    36788987
    36751918
    36434304
    36204927
    35865596
    35667659
    35667587
    35363207
    35363207
    34290017
    34246396
    34246279
    33114693
    33114682
    32970840
    32815113
    32656219
    32655995
    32424902
    31894299
    31275672
    29441440
    29440617
    28577961
    28372002
    28371887
    28181385
    28115930
    27578892
    27427757
    26885688
    26862862
    26407311))
  ((920288 "C-KIT Gene" ("gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "gngm"
   "neop"
   (87384637 51889889 37195338 33819150 33819075))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "AUGMENTS"
   "aapp"
   "neop"
   (48230271))
  ((920288 "C-KIT Gene" ("gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "NEG_ASSOCIATED_WITH"
   "gngm"
   "neop"
   (39466339))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "AUGMENTS"
   "gngm"
   "neop"
   (50049662))
  ((920288 "C-KIT Gene" ("gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "PART_OF"
   "gngm"
   "neop"
   (83399926
    77057096
    76771910
    76770354
    76769868
    68641343
    60522410
    56141218
    54281565
    49025814
    47942137
    45811599
    45436399
    45097136
    44567254
    42421331
    41205270
    40425019
    40347845
    36202499
    31410208))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "aapp"
   "neop"
   (59625558 59480455 43196727 40249098))
  ((920288 "C-KIT Gene" ("gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "PREDISPOSES"
   "gngm"
   "neop"
   (76270951 70297541 51412256 27939940))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "CAUSES"
   "gngm"
   "neop"
   (87384637
    85323758
    85323753
    83221026
    61335391
    58431044
    57775929
    55436679
    54253302
    53789085
    53789078
    52927884
    50274407
    50049773
    49635451
    46841526
    46757085
    46299485
    44187839
    42671642
    41287669
    37195338
    34247527
    33819150
    33819075
    33114772
    32425400
    31543639
    31187106
    27728484
    27728480))
  ((920288 "C-KIT Gene" ("gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "TREATS"
   "aapp"
   "neop"
   (88061705
    85835773
    85241243
    81607888
    61093492
    60354773
    58510784
    58431141
    48276039
    45629899
    42053369
    41029401
    40351927))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "DISRUPTS"
   "gngm"
   "neop"
   (44804518))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "NEG_ASSOCIATED_WITH"
   "gngm"
   "neop"
   (58372927 42789720))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "NEG_PART_OF"
   "gngm"
   "neop"
   (42672081))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "NEG_TREATS"
   "aapp"
   "neop"
   (53583614))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "PART_OF"
   "gngm"
   "neop"
   (88816701
    88504351
    87664119
    86527022
    86527021
    85431031
    84724497
    82274147
    81758346
    81503677
    81503592
    80572441
    80301506
    80113871
    79973963
    79689424
    78726567
    77821336
    77196050
    77196050
    76776826
    76776713
    76548783
    76252420
    75385720
    75385282
    75385190
    74686997
    74324816
    73776735
    73151923
    72884934
    71444438
    70575975
    70335620
    70335615
    69854253
    69854002
    68641343
    67432607
    67356874
    67246601
    67092734
    67092730
    66938421
    66938290
    64461732
    64424903
    63702674
    63702626
    63702530
    63556857
    63556721
    63556717
    63491002
    61959886
    61806767
    61806198
    60522410
    60096404
    60096404
    58546477
    58546256
    58155422
    58155406
    57723933
    56641477
    56216003
    55455524
    55122648
    55122468
    54602884
    54602791
    54281565
    53839416
    52398498
    52189617
    52064489
    52064364
    51366471
    51366401
    51232846
    50359952
    50359900
    49606405
    49527188
    49527185
    49527095
    48065218
    48065150
    48061838
    47942137
    46383271
    46115797
    45811639
    45811639
    45256545
    45105920
    45105498
    45105492
    45097136
    44823923
    44357089
    44356901
    43533383
    43229922
    43066454
    42789714
    42628108
    42119394
    41205270
    40864010
    40674289
    40347845
    40036044
    38954984
    38954930
    38398428
    36262989
    36202499
    35397427
    35363368
    34259788
    31893874
    28372275
    28372177
    28372114
    28372013
    28372007
    28371822))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "PREDISPOSES"
   "aapp"
   "neop"
   (83625667 76441483 64529929 28452099))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "PREDISPOSES"
   "gngm"
   "neop"
   (82889672 76270951 70297541 68769591 67716993 53756432 51412256 27939940))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "PREVENTS"
   "aapp"
   "neop"
   (54419758))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "TREATS"
   "aapp"
   "neop"
   (89381133
    87264300
    84046111
    83347331
    83212313
    83212026
    78726651
    76441724
    73846868
    73616831
    72119603
    71290296
    63973922
    63557099
    62693228
    61564880
    58650608
    58510784
    58431141
    58431135
    57460563
    56215499
    54580204
    53770840
    51513499
    51414870
    50287763
    48710463
    48636064
    45867849
    45629899
    42593053
    40213719
    40162438
    38953465
    38953454
    36434207
    34247121
    32462103
    32462099
    31121291
    27939123))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "compared_with"
   "gngm"
   "neop"
   (42860729))
  ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   "higher_than"
   "gngm"
   "neop"
   (42860733)))
;;
#|
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (fuzzy-concepto "KIT gene" s)
                        (fuzzy-concepto "Gastoirntestinal Stromal Tumors" m)
                        (edgeo e)))))
|#





;; A zillion answers, many with a staggering number of pubmed entries
;;
;; ((4096 "Asthma" ("dsyn"))
;;  (11616 "Contact Dermatitis" ("dsyn"))
;;  "COEXISTS_WITH"
;;  "dsyn"
;;  "dsyn"
;;  (41036104 35093327))
;;
#|
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "asthma" s)]
                          [(fuzzy-concepto "asthma" m)])
                        (edgeo e)))))
|#

;; A zillion answers!
;;
;; ((3147201 "ERVK-2 gene" ("gngm" "aapp"))
;;  (1155074 "mast cell activation" ("celf"))
;;  "AUGMENTS"
;;  "gngm"
;;  "celf"
;;  (20160432))
;;
#|
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "mast cell activation" s)]
                          [(fuzzy-concepto "mast cell activation" m)])
                        (edgeo e)))))
|#

;; A zillion answers!
;;
;; ((238198 "Gastrointestinal Stromal Tumors" ("neop"))
;;  (596290 "Cell Proliferation" ("celf"))
;;  "AFFECTS"
;;  "neop"
;;  "celf"
;;  (76749721 53497234 41583304))
;;
#|
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "Gastrointestinal Stromal Tumor" s)]
                          [(fuzzy-concepto "Gastrointestinal Stromal Tumor" m)])
                        (edgeo e)))))
|#

;; A zillion results!  But none of them are relevant, I think.  Wrong name!
;; Instead, try 'Gastrointestinal Stromal Tumor'
#|
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "GIST" s)]
                          [(fuzzy-concepto "GIST" m)])
                        (edgeo e)))))
|#


;; 'overactivation' doesn't seem to be a thing in SemMedDB.
;; I'm just going to look for any connections between the KIT gene and Gastrointestinal Stromal Tumors;
;; ((17255 "Gene Activation" ("genf"))
;;  (3242 "Antibodies, Anti-Idiotypic" ("aapp" "imft" "gngm"))
;;  "AFFECTS"
;;  "genf"
;;  "aapp"
;;  (27977575))

;; ((14429 "Enzyme Activation" ("moft"))
;;  (1150005 "epoxide hydrolase activity" ("moft"))
;;  "NEG_CAUSES"
;;  "moft"
;;  "moft"
;;  (10230110))

;; ((9528 "Complement Activation" ("moft"))
;;  (1516369 "Cellular Infiltration" ("celf"))
;;  "CAUSES"
;;  "moft"
;;  "celf"
;;  (38511543))
#|
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "overactivation" s)]
                          [(fuzzy-concepto "overactivation" m)])
                        (edgeo e)))))
|#

;; A zillion results.  'KIT gene' and 'C-KIT Gene' show up a ton.  For example:
;;
;; ((1323046 "Detection Kits" ("medd"))
;;  (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
;;  "COEXISTS_WITH"
;;  "medd"
;;  "gngm"
;;  (55842475))
;; ((920288 "C-KIT Gene" ("gngm" "aapp"))
;;  (1335214 "PIK3CG gene" ("gngm" "aapp" "enzy"))
;;  "STIMULATES"
;;  "gngm"
;;  "gngm"
;;  (85338529))
;;
#|
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "Kit" s)]
                          [(fuzzy-concepto "Kit" m)])
                        (edgeo e)))))
|#


;; a zillion answers!
;;
;; (935989 "imatinib" ("phsu" "orch"))
;;
;; (939537 "Imatinib mesylate" ("orch" "phsu"))
#|
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "imatinib" s)]
                          [(fuzzy-concepto "imatinib" m)])
                        (edgeo e)))))
|#
