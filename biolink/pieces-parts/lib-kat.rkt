(define (filter-edges a-drug predicate-or-predicates)
  (define predicates
    (if (pair? predicate-or-predicates)
        predicate-or-predicates
        (list predicate-or-predicates)))
  (define q
    (time
     (query/graph
      ((X a-drug)
       (O #f))
      ((X->O predicates))
      (X X->O O))))
  (map curie-synonyms/names (curies/query q 'O)))


(define (filter-edges a-drug predicates)
  (define q
    (time
     (query/graph
      ((X a-drug)
       (O #f))
      ((X->O predicates))
      (X X->O O))))
  (map curie-synonyms/names (curies/query q 'O)))


(define (side-effects a-drug)
  (filter-edges a-drug (list "causes" "contributes_to")))

(define (phenotypes a-protein)
  (filter-edges a-protein (list "has_phenotype")))

(define (counterindications a-drug)
  (filter-edges a-drug "contraindicated_for"))

(define (indications a-drug)
  (filter-edges a-drug "indicated_for"))

(define (treats a-drug)
  (filter-edges a-drug "treats"))

#;
(side-effects imatinib)
#;
(counterindications imatinib)

(define (get-cars a-list)
  (if (null? a-list) '()
      (append (car a-list) (get-cars (cdr a-list)))))

(define (get-cdr a-list)
  (if (null? a-list) '()
      (cons (cdr (car a-list)) (get-cdr (cdr a-list)))))


(define (list-intersect-2 lst1 lst2)
  (set->list
   (set-intersect (list->set lst1)
                  (list->set lst2))))

; getting side effects for dexamethasone
(define dex-list (side-effects "CUI:C0011777"))
(define dex-list-treats (treats "CUI:C0011777"))
(define all-treats-dex (get-cdr (get-cars dex-list-treats)))
(define all-side-effects-dex (get-cdr (get-cars dex-list)))

; getting phenotypes for random protein from UniProt
(define rand-list (phenotypes "UniProtKB:Q4LDG9"))
(define all-pairs-phenotypes-rand (get-cars rand-list))
(define all-phenotypes-rand (get-cdr all-pairs-phenotypes-rand))

#|
Gets the intersection of the two lists to show which 
("Lung inflammation"
  "Pneumonia NOS"
  "pneumonia"
  "Pneumonia"
  "Pulmonary inflammation"
  "Pulmonitis")
|#

; gene list
(define genes-of-interest
'(
"HGNC:7871"
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
"HGNC:2537"
)
)

(define (encoding-to gene)
  (filter-edges gene "encodes"))

(define (only-uniprot proteins)
  (filter (lambda (x) (string-prefix? x "UniProtKB:")) proteins))

(define all-genes (only-uniprot (map car (get-cars (get-cars (map encoding-to genes-of-interest))))))

(define all-phenotypes (map phenotypes all-genes))

(define all-single-phenotypes (map cdr (get-cars (get-cars all-phenotypes))))

(define side-effects-in-common (list-intersect-2 all-single-phenotypes all-side-effects-dex))
(define treated-in-common (list-intersect-2 all-single-phenotypes all-treats-dex))


