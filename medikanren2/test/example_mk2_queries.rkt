#lang racket/base
(require
 "../base.rkt"
 (prefix-in semmed: "../db/semmed.rkt")
 (prefix-in rtx: "../db/rtx2-20210204.rkt")
 (prefix-in kgx: "../db/kgx-synonym.rkt")
 (prefix-in sri: "../db/sri-reference-kg-0.3.0.rkt")
 racket/pretty)

#|
### Loading and Providing ###

- (prefix-in ...) appends the symbol prefix given (ie "semmed:") onto the materialized relations built in the /db/semmed.rkt file, allowing the user
to define specific relations from specific knowledge graphs for querying. Relations in both the rtx2-20210204.rkt and semmed.rkt files are named
cprop, eprop, edge, meaning that without a prefix, use of  the (cprop ...) relation will query both rtx2 and semmed KGs. 

### KG Specific Relations ### 
* Note: for all KGs with a prefix in the require statement above, the corresponding prefix must be appended to relation for the query to run. 
Shown below are the native relations from each KG *

semmed.rkt
 - cprop
 - eprop
 - edge

rtx2-20210204.rkt
 - cprop
 - eprop
 - edge

sri-reference-kg-0.3.0.rkt
- nodes
- edges

sri-reference-kg-0.3.0.rkt
- synonym


|#

 
;; relation cprop exists in rtx2, semmed
(run 5
     (c k v)
     ;(rtx:cprop c k v)
     (semmed:cprop c k v)
     )

;; relation eprop exists in rtx2, semmed
(run 5
     (c k v)
     ;(rtx:eprop c k v)
     (semmed:eprop c k v)
     )

;; relation edge exists in rtx2, semmed
(run 5
     (c k v)
     ;(rtx:edge c k v)
     (semmed:edge c k v)
     )


;; relation synonym should exist in kgx-synonym
;; Question: Has anyone gotten a response/working query with this KG? 
(run 5
     (a b)
     (kgx:synonym '(a) "CHEBI:28304"))

;; relation nodes should exist in sri-reference KG
(run 5
     (c k v)
     (sri:nodes c k v)
     )

;; relation edges should exist in sri-reference KG
(run 5
     (c k v)
     (sri:edges c k v)
     )

