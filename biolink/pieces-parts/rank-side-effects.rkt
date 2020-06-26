#lang racket
(require
  "../db.rkt"
  "query.rkt"
  "gene-budging.rkt")

(define all-covid-genes
'("HGNC:7871"
"HGNC:4616"
"HGNC:4617"
"HGNC:8554"
"HGNC:8557"
"HGNC:5031"
"HGNC:5033"
"HGNC:7910"
"HGNC:30292"
"HGNC:30291"
"HGNC:10312"
"HGNC:270"
"HGNC:7667"
"HGNC:2734"
"HGNC:10480"
"HGNC:9253"
"HGNC:25242"
"HGNC:3711"
"HGNC:14650"
"HGNC:6897"
"HGNC:14651"
"HGNC:3042"
"HGNC:4897"
"HGNC:13919"
"HGNC:2716"
"HGNC:1471"
"HGNC:24537"
"HGNC:30669"
"HGNC:7113"
"HGNC:12019"
"HGNC:1228"
"HGNC:7114"
"HGNC:9531"
"HGNC:28756"
"HGNC:8869"
"HGNC:24030"
"HGNC:7684"
"HGNC:23663"
"HGNC:25726"
"HGNC:14453"
"HGNC:10405"
"HGNC:16627"
"HGNC:11728"
"HGNC:2712"
"HGNC:2746"
"HGNC:25142"
"HGNC:3275"
"HGNC:3189"
"HGNC:1527"
"HGNC:5960"
"HGNC:12485"
"HGNC:10819"
"HGNC:1125"
"HGNC:790"
"HGNC:7460"
"HGNC:7421"
"HGNC:52028"
"HGNC:9605"
"HGNC:13557"
"HGNC:24591"
"HGNC:1641"
"HGNC:13523"
"HGNC:6118"
"HGNC:6395"
"HGNC:10803"
"HGNC:992"
"HGNC:995"
"HGNC:6943"
"HGNC:991"
"HGNC:990"
"HGNC:10901"
"HGNC:6397"
"HGNC:9438"
"HGNC:1709"
"HGNC:11876"
"HGNC:500"
"HGNC:29620"
"HGNC:3009"
"HGNC:16918"
"HGNC:9583"
"HGNC:1814"
"HGNC:537"
"HGNC:24941"
"HGNC:118"
"HGNC:5270"
"HGNC:5261"
"HGNC:5244"
"HGNC:2232"
"HGNC:10304"
"HGNC:3277"
"HGNC:3272"
"HGNC:7857"
"HGNC:16171"
"HGNC:4181"
"HGNC:10452"
"HGNC:6388"
"HGNC:9554"
"HGNC:11301"
"HGNC:24306"
"HGNC:7629"
"HGNC:20726"
"HGNC:9548"
"HGNC:11753"
"HGNC:9621"
"HGNC:16959"
"HGNC:6400"
"HGNC:11440"
"HGNC:11366"
"HGNC:18479"
"HGNC:23338"
"HGNC:8912"
"HGNC:6204"
"HGNC:11364"
"HGNC:9281"
"HGNC:30615"
"HGNC:3696"
"HGNC:12825"
"HGNC:11766"
"HGNC:11320"
"HGNC:171"
"HGNC:172"
"HGNC:19041"
"HGNC:257"
"HGNC:289"
"HGNC:375"
"HGNC:376"
"HGNC:14434"
"HGNC:14311"
"HGNC:11390"
"HGNC:913"
"HGNC:16902"
"HGNC:1057"
"HGNC:1078"
"HGNC:33814"
"HGNC:1459"
"HGNC:19341"
"HGNC:1492"
"HGNC:1674"
"HGNC:1678"
"HGNC:1776"
"HGNC:1777"
"HGNC:1782"
"HGNC:15483"
"HGNC:1786"
"HGNC:1791"
"HGNC:1991"
"HGNC:1994"
"HGNC:1995"
"HGNC:31736"
"HGNC:19083"
"HGNC:2068"
"HGNC:2071"
"HGNC:13659"
"HGNC:2363"
"HGNC:2454"
"HGNC:2455"
"HGNC:2457"
"HGNC:10637"
"HGNC:2674"
"HGNC:2676"
"HGNC:2700"
"HGNC:2704"
"HGNC:2849"
"HGNC:2850"
"HGNC:2851"
"HGNC:2855"
"HGNC:2857"
"HGNC:3061"
"HGNC:3070"
"HGNC:3071"
"HGNC:3091"
"HGNC:3255"
"HGNC:24649"
"HGNC:3386"
"HGNC:3387"
"HGNC:3388"
"HGNC:3389"
"HGNC:3431"
"HGNC:3655"
"HGNC:3767"
"HGNC:4036"
"HGNC:4119"
"HGNC:4195"
"HGNC:4291"
"HGNC:4545"
"HGNC:20565"
"HGNC:4922"
"HGNC:18360"
"HGNC:15566"
"HGNC:6171"
"HGNC:6193"
"HGNC:6307"
"HGNC:15719"
"HGNC:29798"
"HGNC:698"
"HGNC:20917"
"HGNC:6514"
"HGNC:6524"
"HGNC:18608"
"HGNC:6840"
"HGNC:6843"
"HGNC:6846"
"HGNC:6847"
"HGNC:6849"
"HGNC:6850"
"HGNC:6871"
"HGNC:6886"
"HGNC:19035"
"HGNC:17574"
"HGNC:7110"
"HGNC:7111"
"HGNC:7381"
"HGNC:7529"
"HGNC:16243"
"HGNC:7601"
"HGNC:15576"
"HGNC:7648"
"HGNC:11399"
"HGNC:7850"
"HGNC:18981"
"HGNC:8883"
"HGNC:9404"
"HGNC:9529"
"HGNC:9612"
"HGNC:9618"
"HGNC:9671"
"HGNC:10251"
"HGNC:10812"
"HGNC:6773"
"HGNC:16852"
"HGNC:11394"
"HGNC:11403"
"HGNC:11404"
"HGNC:11406"
"HGNC:16835"
"HGNC:29259"
"HGNC:26160"
"HGNC:11847"
"HGNC:11850"
"HGNC:11904"
"HGNC:17995"
"HGNC:16473"
"HGNC:17797"
"HGNC:487"
"HGNC:9437"
"HGNC:2852"
"HGNC:2537"))

;;not not 
(define (member? elem lst)
  (not (not (member elem lst))))

(define (unwrap lst)
  (if (null? lst) lst
      (append (car lst) (unwrap (cdr lst)))))

;;from lib.rkt
(define (filter-edges a-drug predicates)
  (define q
    (time
     (query/graph
      ((X a-drug)
       (O #f))
      ((X->O predicates))
      (X X->O O))))
  (map curie-synonyms/names (curies/query q 'O)))

;;(side-effects imatinib)
;;(counterindications imatinib)

(define (all-predicates start)
(define q
  (time
   (query/graph
    ((X start)
     (O #f))
    ((X->O #f))
    (X X->O O))))
(sort (remove-duplicates (map (lambda (e) (cdr (list-ref e 4))) (edges/query q 'X->O))) string<=?)
)
;;(all-predicates imatinib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;MONDO:0010602 - hemophilia A
;;MONDO:0010603 - hemophilia A with vascular abnormality

(define (predicate e)
  (cdr (list-ref e 4)))

(define (desired/pred? e l)
  (member (predicate e) l))

#|
(define (filter-edges a-drug an-edge)
  (define qf (query/graph
              ((S a-drug)
                (O #f))
               ((S->O) #f (lambda (e) (string=? an-edge (cdr (list-ref e 4)))))
               (S S->O O)))
  (map curie-synonyms/names (curies/query q 'O)))

|#
#|(define (filter-edges Es pred)
  (if (eq? (length Es) 1) Es
      (if (string=? (predicate (car Es)) pred) (cons (car Es) (filter-edges (cdr Es) pred))
          (filter-edges (cdr Es) pred))))
|#

#|

(define q (query/graph
           ((D drug)
            (O "MONDO:0010602"))
           ((D->O #f))
           (D D->O O)))

(define es (edges/query q 'D->O))
(define preds (map predicate es))
(define ds (curies/query q 'D))
(define dnames (map curie-synonyms/names (curies/query q 'D)))
|#
#|
;;get the CHEMBL synonym
(define (chembl? curie) (string-prefix? curie "CHEMBL.COMPOUND:"))

(define (get-chembl names)
  (if (chembl? (car names)) (car names)
      (get-chembl (cdr names))))
|#
;;CHEMBL.COMPOUND:CHEMBL877 - tranexamic acid
;;CHEMBL.COMPOUND:CHEMBL114 - saquinovir

#|
(define es2 (edges/query saqq 'D->O))
(define preds2 (map predicate es2))
(define obj2 (map curie-synonyms/names (curies/query saqq 'O)))
|#




(define treat-preds (list "treats"
                          "contributes_to"
                          "prevents"
                          "related_to"
                          "targets"
                          "directly_interacts_with"))

(define phen-preds (list "has_phenotye"
                         "disease_has_feature"
                         "has_disposition"
                         "causes_condition"
                         "related_condition"))

(define side-effect-preds (list "causes"
                                "contributes_to"
                                "contraindicated_for"))
#|
(define (treat/pred? e)
  (desired/pred? e treat-preds))

(define (phen/pred? e)
  (desired/pred? e phen-preds))

(define qtreatments (query/graph
           ((D drug)
            (O "MONDO:0010602"))
           ((D->O #f))
           (D D->O O)))

(define qgcauses (query/graph
           ((G gene)
            (O "MONDO:0010602"))
           ((G->O #f))
           (G G->O O)))

(define qpcauses (query/graph
           ((P protein)
            (O "MONDO:0010602"))
           ((P->O #f))
           (P P->O O)))

(define qphens (query/graph
           ((S "MONDO:0010602")
            (O phenotype))
           ((S->O #f))
           (S S->O O)))

(define drugs (map curie-synonyms/names (curies/query qtreatments 'D)))
(define all-drug-synons (remove-duplicates (map car (unwrap drugs))))
(define genes (map curie-synonyms/names (curies/query qgcauses 'G)))
(define prots (map curie-synonyms/names (curies/query qpcauses 'P)))
;;(define phens (map curie-synonyms/names (curies/query qphens 'O)))
|#

(define (find-dups l1 l2)
  (if (or (null? l1) (null? l2)) '()
      (if (member? (car l1) l2) (cons (car l1) (find-dups (cdr l1) l2))
          (find-dups (cdr l1) l2))))
 
;;(define exacerbates (find-dups phens saq-side-effects))

(define (exacerbates/phenos? drg phenos)
  (define side-effects (filter-edges drg side-effect-preds))
  (not (null? (find-dups side-effects phenos))))

(define (exacerbated-phenos drg phenos)
  (define side-effects (filter-edges drg side-effect-preds))
  (if (exacerbates/phenos? drg phenos) (find-dups side-effects phenos) '()))

(define counter (make-hash))

(define (count-num-side-effects drgs)
  (hash-clear! counter)
  (if (null? drgs) (void)
      (for-each (lambda (e) (hash-set! counter e (length (filter-edges e side-effect-preds))))
                drgs)))

(define (rank-num-side-effects drgs)
  (count-num-side-effects drgs)
  (define counted (hash->list counter))
  (sort counted #:key cdr <=))

(define (rank-drugs-related-to disease-curie patient-phens)
  (define qtreats (query/graph
                   ((D drug)
                    (O disease-curie))
                   ((D->O #f))
                   (D D->O O)))
  ;;drugs that somehow interact with given disease
  (define drugs (map curie-synonyms/names (curies/query qtreats 'D)))
  ;;all those drug's synonyms
  (define all-drug-synons (remove-duplicates (map car (unwrap drugs))))

  (define qphens (query/graph
                  ((S disease-curie)
                   (O phenotype))
                  ((S->O #f))
                  (S S->O O)))
  ;;phenotypes of given disease
  (define phens (map curie-synonyms/names (curies/query qphens 'O)))

  ;;bottom ranking all drugs that exacerbate the patient's pre-existing conditions/phenotypes
  (define affects-patient (remf* null? (map (lambda (x) (if (exacerbates/phenos? x patient-phens) x '())) all-drug-synons)))

  ;;bottom ranking all drugs that exacerbate phenotypes of given disease
  (define bottom-rank (remf* null? (map (lambda (x) (if (exacerbates/phenos? x phens) x '())) all-drug-synons)))

  ;;ranking all of the drugs within those three categories based on how many side effects they have
  (define top-rank (remove* (append bottom-rank affects-patient) all-drug-synons))

  (list (rank-num-side-effects top-rank) (rank-num-side-effects bottom-rank) (rank-num-side-effects affects-patient))
 )

;;TODO: create method that takes a list of drugs and ranks based on number of side effects
  ;;apply to all three portions of rank, then append them all together
  ;;try on a coronavirus curie
  ;;if time,
  ;;figure out a way to rank the drugs that are exacerbate both disease and patient last
  ;;figure out way to report the edges that show which drugs exacerbate which phenotypes

#| want ranking to go
don't exacerbate disease (ranked most side effects to least)
exacerbate patient's condition, but not disease (ranked most patient conditions affected to least)
exacerbates disease (ranked most side effects to least)
exacerbates both patient condition and disease
|#
  
;;(find-dups '(1 2 3 4 5 6 7 8) '(2 4 6 8 10 12))