#lang racket/base

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level-multi-db.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers-multi-db.rkt"
 racket/set)

;; Numbers of the top bucket of the RoboKop KG, Text Mining KG, and RTX-KG2 KG.
(define TOP_BUCKET_NUMBERS (list (list (get-highest-bucket-number-robokop))
                                 (list (get-highest-bucket-number-text-mining))
                                 (list (get-highest-bucket-number-rtx-kg2))))

;; a comparison between one-hop query with or without using the bucket setting
(define regulates-EGFR
  (time (query:X->Known
         #f
         '("biolink:regulates")
         (curies->synonyms-in-db (list "HGNC:3236")))))
(length regulates-EGFR)
; 44

(define regulates-EGFR-faster
  (time (query:X->Known-scored
         #f
         '("biolink:regulates")
         (curies->synonyms-in-db (list "HGNC:3236"))
         TOP_BUCKET_NUMBERS)))
(length regulates-EGFR-faster)
; 0

;; There is a problem when we use the bucketing approach is that we might 
;; receive zero answer from the buckets that has a higer score (amount of supports).
;; Hence, you can either manually decrease the bucket number and redo the
;; query, or you may write a procedure to realize auto growing until reach
;; the amount of answers you want, like the procedure auto-grow shown below.


;; The procedure 1-hop-proc takes a list of bucket numbers.
(define (1-hop-proc bucket*)
  (query:X->Known-scored
         #f
         '("biolink:regulates")
         (curies->synonyms-in-db (list "HGNC:3236"))
         bucket*))

;; The procedure auto-grow takes a query procedure (how do you want to
;; query against the mediKanren neo server?), a list of buket numbers to
;; start with, and a number representing the least amount of anwers
;; you expected to receive. It is possible you do not get 'enough'
;; answers, it is because those are all the answers it can return
;; from the KGs. You may try to modify the 1-hop-proc to get more:
;; more input curies, taking the subclasses of it/them, etc. 
(define example (time (auto-grow 1-hop-proc TOP_BUCKET_NUMBERS 100)))
(length example)
; 44


;; The implementation of auto-grow is constomized with the need of the
;; mediKanren neo-server. So it starts querying with the given
;; list of bucket numbers and decreases the level of confidency (score of
;; the edge) each round. It does not have to behavior this way. You may
;; write your own procedure to achieve a different goal.

;; A trick to not query against a specific KG can be achived by manually
;; set the bucket number to be #f, like (#f '(10) '(10)) which means
;; 'I would not like answers from the robokop KG, and please querying
;; text-mining KG and rtx-kg2 KG with the bucket 10 for each.'. As you
;; may have noticed from the definition of TOP_BUCKET_NUMBERS, the bucket list
;; represents the KGs in the order of robokop, text-mining, and rtx-kg2.
(define regulates-EGFR-ramdom
  (time (query:X->Known-scored
         #f
         '("biolink:regulates")
         (curies->synonyms-in-db (list "HGNC:3236"))
         (list #f '(10) '(10)))))
(length regulates-EGFR-ramdom)
; 0
