#lang racket
(provide (all-defined-out))
(require "../../../medikanren/pieces-parts/query.rkt"
         racket/engine)
;;not druggable 
#;(define all-hgncs
  '("HGNC:894"))

;; HGNC:892, HGNC:1270 error in the constraint solver 

#;(define all-hgncs
  (file->list "/Users/michaelpatton/git/mediKanren/biolink/data/all-hgnc-curies.scm"))

#;(define ARDS-gene-ls
  (map (lambda (x) (string-append "HGNC:" x))
       (map number->string
            '(4818
              7516
              8583
              320
              1936
              2766
              3655
              5992
              6000
              6014
              6018
              5962
              5973
              5981
              5986
              16830
              17020
              6709
              6922
              7097
              7797
              8947
              10798
              10799
              10803
              11362
              17192
              11847
              11892
              11908
              12036
              3542
              4443
              21581
              9052
              12726
              2707
              333
              338
              3374
              3763
              5273
              6363
              6848
              6858
              7590
              30092
              12680
              1232
              3999
              5013
              5014
              7782
              2874
              11181
              11920
              186
              13633
              348
              600
              1550
              2595
              2884
              3553
              9237
              9386
              12554
              12698
              4603
              6027
              4035
              4053
              51
              290
              17731
              4095
              4595
              5293))))


#|
(define anti-coag-list
  '("RXNORM:11289"
    "CUI:C0043031" 
    "RXNORM:1546356"
    "CUI:C2348066" 
    "RXNORM:1114195"
    "CUI:C1739768" 
    "RXNORM:1364430"
    "CUI:C1831808" 
    "RXNORM:1599538"
    "CUI:C2975435" 
    "RXNORM:280611"
    "CUI:C0916062" 
    "RXNORM:152610"
    "CUI:C0592532" 
    "RXNORM:67109"
    "CUI:C0206461"
    "RXNORM:67108"
    "CUI:C0206460" 
    "RXNORM:67031"
    "CUI:C0206232" 
    "RXNORM:69528"
    "CUI:C0215717" 
    "RXNORM:75960"
    "CUI:C0254215" 
    "RXNORM:69646"
    "CUI:C0216278" 
    "RXNORM:5224"
    "CUI:C0019134"))
|#
#|
;; EWSR1 genes
(define EWSR1-gene-ls
  (map (lambda (x) (string-append "HGNC:" x))
       (map number->string
            '(38
              14899
              943
              320
              19990
              489
              490
              2013
              638
              14103
              15607
              14872
              974
              24176
              14343
              1325
              1389
              1537
              26295
              1705
              20455
              2174
              2191
              22990
              2333
              24292
              2498
              2530
              20348
              2907
              2908
              2926
              3010
              3016
              32188
              19087
              3154
              27234
              27683
              30114
              34236
              3603
              13590

              3761
              3800
              3801
              3817
              25491
              4092
              4122
              4131
              4882
              5141
              17087
              5466
              25251
              23236
              48664
              6617
              6664
              16895
              17200
              14575
              17082
              7159
              7171
              7514
              7614
              29832
              13395
              7746
              7775
              16191
              19321
              27106
              7909
              41983
              7952
              8126
              8134
              19316
              8604
              17270
              16068
              14180
              27559
              8890
              8906
              30065
              19255
              29037
              14683

              26303
              20331
              688
              20457
              13441
              13655
              24188
              32455
              18386
              18387
              10798
              10799
              10801
              10802
              10803
              16192
              11046
              11049
              11073
              16698
              15592
              18065
              15885
              11568
              11581
              11595
              20329
              27916
              11950
              30887
              20670
              29315
              15466
              12769
              19221
              16804
              13083))))

|#




#|proviral gene list for 2-hop|#
;;"HGNC:11876" TMPRSS2
;;"HGNC:8568" FURIN

;; ANO1 for haley
;; 21625 ano1
;; 2244 = 24 genes
#|
(define pro-viral-gene-list
  '())
|#
#|
"HGNC:15631"
    "HGNC:15632"
    "HGNC:11876"
    "HGNC:8568"
    "HGNC:13557"
    "HGNC:3009"
|#


#|
;; alzheimer's genes                    ; ;
(define pro-viral-gene-list             ; ;
  (map symbol->string                   ; ;
       '(HGNC:1052
         HGNC:2095
         HGNC:613
         HGNC:37                        ; ;
         HGNC:2334                      ; ;
         HGNC:15514                     ; ;
         HGNC:13375                     ; ;
         HGNC:1659                      ; ;
         HGNC:14284                     ; ;
         HGNC:14258                     ; ;
         HGNC:620                       ; ;
         HGNC:6893                      ; ;
         HGNC:9508                      ; ;
         HGNC:9509                      ; ;
         HGNC:17761)))                           ; ;
|#


#|
(define pro-viral-gene-list             ; ;
'("HGNC:11876"                          ; ;
"HGNC:8568"                             ; ;
"HGNC:7850"                             ; ;
"HGNC:11320"                            ; ;
"HGNC:8883"                             ; ;
"HGNC:1776"                             ; ;
"HGNC:2232"                             ; ;
"HGNC:2537"                             ; ;
))                                      ; ;
|#
;; just TMPRSS2
#;(define pro-viral-gene-list
  '("HGNC:11876"))
#|
(define pro-viral-gene-list
  '("CHEBI:6887"))
|#

#|
#|
;pro-viral kinases
NME2
ABI1
PFTK1
CDK5R2
COPB2
CTSL
|#

(define pro-viral-gene-list
'("HGNC:7850"
"HGNC:11320"
"HGNC:8883"
"HGNC:1776"
"HGNC:2232"
"HGNC:2537"))
|#

#|
;;anti-viral kinases
CLK1
MAP2K6
CSNK1G1
EPHA3
CDK6
AURKB
GCK
DGKD
|#
#|
;; short 2hop "HGNC:8122"
(define pro-viral-gene-list
  '("HGNC:24591"))
|#
#;(define pro-viral-gene-list
  '("HGNC:24591"
    "HGNC:19356"
    "HGNC:10941"
    "HGNC:51"
    "HGNC:28756"
    "HGNC:76"
    "HGNC:2707"
    "HGNC:13557"
    "HGNC:7976"
    "HGNC:338"
    "HGNC:790"
    "HGNC:868"
    "HGNC:864"
    "HGNC:13919"
    "HGNC:990"
    "HGNC:991"
    "HGNC:992"
    "HGNC:995"
    "HGNC:1103"
    "HGNC:1125"
    "HGNC:25142"
    "HGNC:1471"
    "HGNC:1527"
    "HGNC:1641"
    "HGNC:1859"
    "HGNC:24537"
    "HGNC:2228"
    "HGNC:2230"
    "HGNC:2231"
    "HGNC:2232"
    "HGNC:2234"
    "HGNC:2237"
    "HGNC:2243"
    "HGNC:9605"
    "HGNC:7421"
    "HGNC:52028"
    "HGNC:2459"
    "HGNC:2460"
    "HGNC:2552"
    "HGNC:2712"
    "HGNC:28777"
    "HGNC:2716"
    "HGNC:2746"
    "HGNC:2976"
    "HGNC:3189"
    "HGNC:3277"
    "HGNC:3275"
    "HGNC:3272"
    "HGNC:3293"
    "HGNC:3723"
    "HGNC:18169"
    "HGNC:3538"
    "HGNC:23397"
    "HGNC:3711"
    "HGNC:33276"
    "HGNC:8568"
    "HGNC:14375"
    "HGNC:4181"
    "HGNC:19687"
    "HGNC:4296"
    "HGNC:4616"
    "HGNC:14453"
    "HGNC:4853"
    "HGNC:4897"
    "HGNC:5031"
    "HGNC:24921"
    "HGNC:5960"
    "HGNC:6053"
    "HGNC:6118"
    "HGNC:6204"
    "HGNC:6395"
    "HGNC:29531"
    "HGNC:25726"
    "HGNC:6664"
    "HGNC:29620"
    "HGNC:3332"
    "HGNC:6897"
    "HGNC:6943"
    "HGNC:18873"
    "HGNC:7113"
    "HGNC:7114"
    "HGNC:3942"
    "HGNC:7460"
    "HGNC:7684"
    "HGNC:18591"
    "HGNC:25242"
    "HGNC:7910"
    "HGNC:16885"
    "HGNC:8068"
    "HGNC:3255"
    "HGNC:8869"
    "HGNC:8912"
    "HGNC:9437"
    "HGNC:9281"
    "HGNC:14650"
    "HGNC:9380"
    "HGNC:9531"
    "HGNC:17822"
    "HGNC:9828"
    "HGNC:3402"
    "HGNC:10019"
    "HGNC:10312"
    "HGNC:10480"
    "HGNC:18276"
    "HGNC:1228"
    "HGNC:10803"
    "HGNC:8157"
    "HGNC:10905"
    "HGNC:13621"
    "HGNC:30669"
    "HGNC:30615"
    "HGNC:11364"
    "HGNC:11728"
    "HGNC:11766"
    "HGNC:11849"
    "HGNC:16627"
    "HGNC:2236"
    "HGNC:6021"
    "HGNC:11850"
    "HGNC:11876"
    "HGNC:12019"
    "HGNC:12458"
    "HGNC:12666"
    "HGNC:23663"
    "HGNC:12825"
    "HGNC:11320"
    "HGNC:171"
    "HGNC:172"
    "HGNC:19041"
    "HGNC:375"
    "HGNC:376"
    "HGNC:52650"
    "HGNC:1078"
    "HGNC:33814"
    "HGNC:1459"
    "HGNC:1674"
    "HGNC:1776"
    "HGNC:15483"
    "HGNC:1994"
    "HGNC:19083"
    "HGNC:2068"
    "HGNC:2700"
    "HGNC:2855"
    "HGNC:3061"
    "HGNC:3255"
    "HGNC:24649"
    "HGNC:3767"
    "HGNC:4922"
    "HGNC:18360"
    "HGNC:6193"
    "HGNC:6514"
    "HGNC:6524"
    "HGNC:6843"
    "HGNC:6846"
    "HGNC:19035"
    "HGNC:7381"
    "HGNC:7529"
    "HGNC:7648"
    "HGNC:9404"
    "HGNC:9612"
    "HGNC:9618"
    "HGNC:9671"
    "HGNC:11403"
    "HGNC:11404"
    "HGNC:11406"
    "HGNC:11904"
    "HGNC:9437"))

(define anti-viral-gene-list
  '("HGNC:3009"
    "HGNC:4617"
    "HGNC:7114"
    "HGNC:8984"
    "HGNC:14650"
    "HGNC:10819"
    "HGNC:12458"
    "HGNC:257"
    "HGNC:289"
    "HGNC:376"
    "HGNC:14311"
    "HGNC:11390"
    "HGNC:913"
    "HGNC:16902"
    "HGNC:1057"
    "HGNC:19341"
    "HGNC:1492"
    "HGNC:1678"
    "HGNC:1777"
    "HGNC:1782"
    "HGNC:1786"
    "HGNC:1791"
    "HGNC:1991"
    "HGNC:31736"
    "HGNC:1995"
    "HGNC:2071"
    "HGNC:13659"
    "HGNC:2363"
    "HGNC:2454"
    "HGNC:2455"
    "HGNC:2457"
    "HGNC:10637"
    "HGNC:2674"
    "HGNC:2676"
    "HGNC:2704"
    "HGNC:2849"
    "HGNC:2850"
    "HGNC:2851"
    "HGNC:2857"
    "HGNC:3070"
    "HGNC:3071"
    "HGNC:3091"
    "HGNC:3386"
    "HGNC:3387"
    "HGNC:3388"
    "HGNC:3389"
    "HGNC:3431"
    "HGNC:3655"
    "HGNC:4036"
    "HGNC:4119"
    "HGNC:4195"
    "HGNC:4291"
    "HGNC:4545"
    "HGNC:20565"
    "HGNC:15566"
    "HGNC:6171"
    "HGNC:6307"
    "HGNC:15719"
    "HGNC:29798"
    "HGNC:18608"
    "HGNC:6840"
    "HGNC:6847"
    "HGNC:6849"
    "HGNC:6850"
    "HGNC:6871"
    "HGNC:6886"
    "HGNC:17574"
    "HGNC:7110"
    "HGNC:7111"
    "HGNC:16243"
    "HGNC:7601"
    "HGNC:15576"
    "HGNC:11399"
    "HGNC:7850"
    "HGNC:18981"
    "HGNC:8883"
    "HGNC:9529"
    "HGNC:10251"
    "HGNC:10812"
    "HGNC:6773"
    "HGNC:16852"
    "HGNC:11394"
    "HGNC:16835"
    "HGNC:29259"
    "HGNC:26160"
    "HGNC:11847"
    "HGNC:17995"
    "HGNC:16473"
    "HGNC:17797"
    "HGNC:487"
    "HGNC:2852"))



#|HELPER FUNCTIONS|#
(define inner-loop/csv
  (lambda (ls port)
    (cond
      ((null? ls) (void))
      (else
       (fprintf port "~a~c" (car ls) #\,)
       (inner-loop/csv (cdr ls) port)))))

(define outer-loop/csv
  (lambda (ls port)
    (cond
      ((null? ls)
       (close-output-port port))
      (else
       (inner-loop/csv (car ls) port)
       (fprintf port (format "~c" #\newline))
       (outer-loop/csv (cdr ls) port)))))

(define export-column-headers/csv
  (lambda (headers port path)
    (cond 
      ((= (file-size path) 0)
       (cond
         ((null? headers)
          (fprintf port "~c" #\newline))
         (else
          (fprintf port "~a~c" (car headers) #\,)
          (export-column-headers/csv (cdr headers) port path))))
      (else
       (void)))))


(define export-column-headers
  (lambda (headers port path)
    (cond 
      ((= (file-size path) 0)
       (cond
         ((null? headers)
          (fprintf port "~c" #\newline))
         (else
          (fprintf port "~a~c" (car headers) #\tab)
          (export-column-headers (cdr headers) port path))))
      (else
       (void)))))

(define outer-loop
  (lambda (ls port)
    (cond
      ((null? ls)
       (close-output-port port))
      (else
       (inner-loop (car ls) port)
       (fprintf port (format "~c" #\newline))
       (outer-loop (cdr ls) port)))))

(define inner-loop
  (lambda (ls port)
    (cond
      ((null? ls) (void))
      (else
       (fprintf port "~a~c" (car ls) #\tab)
       (inner-loop (cdr ls) port)))))



(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (list? x)))))

(define member?
  (lambda (x ls)
    (cond
      ((null? ls) #f)
      (else 
       (or (equal? x (car ls))
           (member? x (cdr ls)))))))

(define union
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) ls2)
      ((member? (car ls1) ls2)
       (union (cdr ls1) ls2))
      (else
       (cons (car ls1)
             (union (cdr ls1) ls2))))))

(define intersect?
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) #f)
      ((member? (car ls1) ls2) #t)
      (else
       (intersect? (cdr ls1) ls2)))))

(define intersect
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) '())
      ((member? (car ls1) ls2)
       (cons (car ls1)
             (intersect (cdr ls1) ls2)))
      (else
       (intersect (cdr ls1) ls2)))))

#|ASSOCIATION-LIST KEYS/STRING LISTS FOR KNOWLEDGE GRAPH CONCEPTS|#
(define robokop-concept-key/eq-id "equivalent_identifiers")
(define robokop-xref-key "equivalent_identifiers")
(define orange-concept-key/synonym "synonym")
(define orange-concept-key/same-as "same_as")
(define semmed-concept-key/xrefs "xrefs")
(define semmed-concept-key/id "id")
(define publication-key "publications")
(define publication-key/pmids "pmids")
(define drug-bank/key "drugbank.groups") 
(define drug-bank/withdrawn/key "withdrawn_flag")
(define drug-bank/approved/key "drugbank.approved")
(define drug-bank/therapeutic/key "therapeutic_flag")
(define drug-bank/investigational/key "drugbank.investigational")
(define umls-type-label/key "umls_type_label")
(define umls-type/key "umls_type")
(define PUBMED_URL_PREFIX "https://www.ncbi.nlm.nih.gov/pubmed/")

(define decreases-pred-str/ls
  '("negatively_regulates"
    "negatively_regulates__entity_to_entity"
    "decreases_molecular_interaction"
    "decreases_secretion_of"
    "decreases_localization_of"
    "decreases_stability_of"
    "decreases_synthesis_of"
    "decreases_transport_of"
    "decreases_uptake_of"    
    "prevents"
    "treats"
    "disrupts"
    "inhibits"
    "inhibitor"
    "binder"
    "antagonist"
    "blocker"
    "increases_degradation_of"
    "decreases_activity_of"
    "decreases_expression_of"))

(define increases-pred-str/ls
  '("positively_regulates"
     "produces"
     "agonist"
     "causes"
     "causes_condition"
     "causally_related_to"
     "contributes_to"
     "gene_associated_with_condition"
     "positively_regulates__entity_to_entity"
     "gene_mutations_contribute_to"
     "decreases_degradation_of"
     "increases_activity_of"
     "increases_expression_of"
     "increases_molecular_interaction"
     "increases_response_to"
     "increases_secretion_of"
     "increases_stability_of"
     "increases_synthesis_of"
     "increases_transport_of"
     "increases_uptake_of"
     "increases_localization_of"
     "stimulates"
     "activator"))

(define predicate-str/ls
  '("physically_interacts_with"
    "regulates"
    "interacts_with"
    "targets"
    "modulator"
    "directly_interacts_with"
    "regulates_expression_of"
    "affects_expression_of"
    "affects"
    "affects_activity_of"
    "uses"))

(define allowable-predicates
  '("negatively_regulates"
    "negatively_regulates__entity_to_entity"
    "decreases_molecular_interaction"
    "decreases_secretion_of"
    "decreases_localization_of"
    "decreases_stability_of"
    "decreases_synthesis_of"
    "decreases_transport_of"
    "decreases_uptake_of"    
    "prevents"
    "treats"
    "disrupts"
    "inhibits"
    "inhibitor"
    "binder"
    "antagonist"
    "blocker"
    "increases_degradation_of"
    "decreases_activity_of"
    "decreases_expression_of"
    "positively_regulates"
    "produces"
    "agonist"
    "causes"
    "causes_condition"
    "causally_related_to"
    "contributes_to"
    "gene_associated_with_condition"
    "positively_regulates__entity_to_entity"
    "gene_mutations_contribute_to"
    "decreases_degradation_of"
    "increases_activity_of"
    "increases_expression_of"
    "increases_molecular_interaction"
    "increases_response_to"
    "increases_secretion_of"
    "increases_stability_of"
    "increases_synthesis_of"
    "increases_transport_of"
    "increases_uptake_of"
    "increases_localization_of"
    "stimulates"
    "activator"
    "physically_interacts_with"
    "regulates"
    "interacts_with"
    "targets"
    "modulator"
    "directly_interacts_with"
    "regulates_expression_of"
    "affects_expression_of"
    "affects"
    "affects_activity_of"
    "uses"))

(define substitute
  (lambda (ls old new)
    (cond 
      ((null? ls) '())
      ((void? (car ls))
       (substitute (cdr ls) old new))
      ((boolean? (car ls))
       (cons
         (format "~a" (car ls))
         (substitute (cdr ls) old new))) 
      ((equal? (car ls) old)
       (cons new
             (substitute (cdr ls) old new)))
      (else
       (cons (car ls)
             (substitute (cdr ls) old new))))))

(define str-converter
  (lambda (ls)
    (cond
      ((null? ls)
       (substitute ls '() "NA"))
      ((or (boolean? (car ls))
           (void? (car ls)))
       (str-converter (cdr ls)))
      (else 
       (if (symbol? (car ls))
           (string-join (map symbol->string ls) " ")
           (string-join ls " "))))))

(define append-predicate-symbol
  (lambda (pred-str inc-pred dec-pred els)
    (cond
      ((null? pred-str) (car els))
      ((or (boolean? (car pred-str))
           (void? (car pred-str)))
       (append-predicate-symbol
        (cdr pred-str) inc-pred dec-pred els))
      ((member? (car pred-str) inc-pred)
       (append-predicate-symbol
        (cdr pred-str)
        inc-pred
        dec-pred
        (cons
         "1" els)))
      ((member? (car pred-str) dec-pred)
        (append-predicate-symbol
         (cdr pred-str)
         inc-pred
         dec-pred
         (cons
          "-1" els)))
      ((not (member? (car pred-str) dec-pred))
       (append-predicate-symbol
        (cdr pred-str)
         inc-pred
         dec-pred
         (cons
          "0" els)))
      (else
       (error (format "PREDICATE NOT A MEMBER OF GLOBAL VARIABLE:\n ~a" (car pred-str)))))))


;;og
#;(define filter/curie
  (lambda (ls els curie)
    (cond
      ((null? ls) (set-union els))
      ((string-prefix? (car ls) curie)
       (filter/curie
        (cdr ls)
        (cons (car ls) els) curie))
      (else
       (filter/curie (cdr ls) els curie)))))
;; test
;; add clause 
(define filter/curie
  (lambda (ls els curie)
    (cond      
      ((null? ls) els)
      ((string-prefix? (car ls) curie)
       (filter/curie
        (cdr ls)
        (cons (car ls) els) curie))
      (else
       (filter/curie (cdr ls) els curie)))))

#|
(define get-gene-synonyms-from-edge-subject-id
  (lambda (subject-id/str)
    (if (string-contains? subject-id/str ":")
        (set->list (curie-synonyms subject-id/str))
        '())))
|#
(define remove-item
  (lambda (x ls els)
    (cond
      ((null? ls) (reverse els))
      ((or (boolean? (car ls))
           (void? (car ls)))
       (remove-item
        x (cdr ls) els))
      ((equal? x (car ls))
       (remove-item x (cdr ls) els))
      (else
       (remove-item x (cdr ls)
                    (cons (car ls) els))))))

#|Gregs code for drug-info |#

(define curie-to-anything
  (lambda (curie predicate*)
    ;;(printf "starting curie-to-anything with curie ~s and preds ~s\n" curie predicate*)
    (let ((val (query/graph
                ( ;; concepts
                 (X curie)
                 (T #f))
                ;; edges
                ((X->T predicate*))
                ;; paths      
                (X X->T T))))
      ;;(printf "finished curie-to-anything with curie ~s and preds ~s\n" curie predicate*)
      val)))

(define curie-to-tradenames
  (lambda (curie)
    (curie-to-anything curie '("has_tradename"))))

(define curie-to-clinical-trials
  (lambda (curie)
    (curie-to-anything curie '("clinically_tested_terminated_phase_2"
                               "clinically_tested_approved_unknown_phase"
                               "clinically_tested_terminated_phase_2"
                               "clinically_tested_terminated_phase_3"
                               "clinically_tested_terminated_phase_2_or_phase_3"
                               "clinically_tested_withdrawn_phase_3"
                               "clinically_tested_withdrawn_phase_2_or_phase_3"
                               "clinically_tested_withdrawn_phase_2"
                               "clinically_tested_suspended_phase_2"
                               "clinically_tested_suspended_phase_3"
                               "clinically_tested_suspended_phase_2_or_phase_3"))))

(define curie-to-indicated_for
  (lambda (curie)
    (curie-to-anything curie '("indicated_for"))))

(define curie-to-contraindicated_for
  (lambda (curie)
    (curie-to-anything curie '("contraindicated_for"))))


(define drug-info-for-curie
  (lambda (curie)
    ; (printf "*** starting drug-info-for-curie ~s\n" curie)
    (map
     (lambda (l)
       (match l
         [`(,name . ,q)
          ; (printf "*** calculating curie-synonyms/names list for curie ~s\n" curie)
          (let ((ls (map curie-synonyms/names (curies/query q 'T))))
            ; (printf "*** calculated curie-synonyms/names list for curie ~s\n" curie)
            ; (printf "*** ls length = ~s\n" (apply + (map length ls)))
            (cons name ls))]))
     (list 
      (cons 'tradenames (curie-to-tradenames curie))
      (cons 'clinical-trials (curie-to-clinical-trials curie))
      (cons 'indicated_for (curie-to-indicated_for curie))
      (cons 'contraindicated_for (curie-to-contraindicated_for curie))))))





;; og works
#;(define get-canonical-curie-from-synonyms-ls
  (lambda (ls)
    (if (null? ls)
        "NA"
        (take ls 1))))

;;testing
(define get-canonical-curie-from-synonyms-ls
  (lambda (x)
    (cond
      ((boolean? x)
       "NA")
      ((null? x) "NA")
      (else
       (car (take x 1))))))

(define get-hgnc-symbol/take
  (lambda (x)
    (cond
      ((or (equal? x '#f)
           (boolean? x))
       (take '("NA") 1))
      ((null? x)
       (take '("NA") 1))
      (else
       (car (string-split (car x) " "))))))

(define get-fda-tag
  (lambda (x)
    (if (string-prefix? x "RXNORM:")
        "YES"
        "NA")))

;; testing with edge match
;;

;; if an edge has subject/object that is a drug/gene
;; we want to get a canonical identifier for that drug/gene
;; drugs dont have a "canonical 1-size-fits-all id"
;; but DRUGBANK:/RXNORM: is good for FDA approval
;; and CHEBI is good for general molecules of biological interest

(define gene-concept-in-synonyms-ls?
  (lambda (synonyms-ls)
    (cond      
      ((null? synonyms-ls) #f)
      (else
       (or (string-prefix? (car synonyms-ls) "HGNC:")
           (gene-concept-in-synonyms-ls? (cdr synonyms-ls)))))))

(define drug-concept-in-synonyms-ls?
  (lambda (synonyms-ls)
    (cond      
      ((null? synonyms-ls) #f)
      (else
       (or (string-prefix? (car synonyms-ls) "CHEBI:")
           (drug-concept-in-synonyms-ls? (cdr synonyms-ls)))))))



(define get-curie-synonyms-from-edge-subject/object-curie
  (lambda (x)
    (cond
      ((string-contains? x ":")
       (let ((synonyms-ls (set->list (curie-synonyms x))))
         (cond
           ((gene-concept-in-synonyms-ls? synonyms-ls)
            (get-canonical-curie-from-synonyms-ls
             (filter/curie synonyms-ls '() "HGNC:")))
           ((drug-concept-in-synonyms-ls? synonyms-ls) 
            (get-canonical-curie-from-synonyms-ls
             (filter/curie synonyms-ls '() "CHEBI:")))
           (else
            "NA"))))
      (else
       "NA"))))

(define extract-symbol-name-from-concept-or-edge
  (lambda (query-ls els)
    (cond
      ((null? query-ls) (set-union els))
      ((or (boolean? (car query-ls))
           (void? (car query-ls)))
       (extract-symbol-name-from-concept-or-edge
        (cdr query-ls) els))
      (else 
       (match (car query-ls)
         [`(,db ,cui ,id ,name ,category ,properties-list)
          (extract-symbol-name-from-concept-or-edge
           (cdr query-ls)
           (set-union
            (list name) els))]
         [`(,db ,edge-cui
                (,subject-cui ,subject-id ,subject-name (,_ . ,subject-category) ,subject-props-assoc)
                (,concept-cui ,concept-id ,concept-name (,_ . ,concept-category) ,concept-props-assoc)
                (,_ . ,pred)
                ,pred-props-assoc)
          (extract-symbol-name-from-concept-or-edge (cdr query-ls)
                                                    (set-union
                                                     (list subject-name) els))])))))

(define query-hgnc-curie-for-hgnc-symbol
  (lambda (ls els)
    (cond
      ((null? ls)
       (remove-duplicates (flatten els)))
      ((or (boolean? (car ls))
           (void? (car ls)))
       (query-hgnc-curie-for-hgnc-symbol
        (cdr ls) els))
      ((string-contains? (car ls) "HGNC:")
       (query-hgnc-curie-for-hgnc-symbol
        (cdr ls)
        (cons
         (extract-symbol-name-from-concept-or-edge
          (find-concepts #t (list (car ls))) '())
         els)))
      (else
       (query-hgnc-curie-for-hgnc-symbol
        (cdr ls) els)))))

(define get-xrefs 
  (lambda (key properties-ls)
    (cond 
      [(assoc key properties-ls)
       => (lambda (assoc-ls)
            (define x-refs (cdr assoc-ls))
            (define in (open-input-string x-refs))
            (cons (read in) '()))]
      (else
       '()))))

(define get-rxnorm
  (lambda (ls els)
    (cond
      ((null? ls) (set-union els))
      ((or (boolean? (car ls))
           (void? (car ls)))
       (get-rxnorm (cdr ls)
                   (cons "NA" els)))
      ((atom? (car ls))
       (cond
         ((regexp-match #rx"[Rr][Xx][Nn][Oo][Rr][Mm]:" (car ls))
          (get-rxnorm (cdr ls)
                      (set-union
                       (cons (car ls) els))))
         (else
          (get-rxnorm (cdr ls) els))))
      (else
       (set-union (get-rxnorm (car ls) els)
                  (get-rxnorm (cdr ls) els))))))

(define get-assoc-value
  (lambda (key assoc-ls)
    (cond
      ((assoc key assoc-ls)
       => (lambda (assoc-ls)
            (let ((assoc-key (car assoc-ls))
                  (assoc-value (cdr assoc-ls)))
              (if (equal? assoc-key key)
                  assoc-value
                  "NA"))))
      (else
       '()))))


(define match-edge/with-S/O-gene-linker-ids
  (lambda (edges-ls els)
    (cond
      ((null? edges-ls) els)
      (else 
       (match (car edges-ls)                       
         [`(,db ,edge-cui
                (,subject-cui ,subject-id ,subject-name (,_ . ,subject-category) ,subject-props-assoc)
                (,object-cui ,object-id ,object-name (,_ . ,object-category) ,object-props-assoc)
                (,_ . ,pred) ,pred-props-assoc)
          (match-edge/with-S/O-gene-linker-ids           
           (cdr edges-ls)
           (cons
            (substitute
             (list
              edge-cui ;; 0 edge-id
              db                        ;; 1 kg id
              (get-canonical-curie-from-synonyms-ls
               (query-hgnc-curie-for-hgnc-symbol
                (list (get-curie-synonyms-from-edge-subject/object-curie subject-id)) '())) ;; 2 linker_curie_nameS
              (get-curie-synonyms-from-edge-subject/object-curie subject-id) ;; 3 linker_curie_idS
              (append-predicate-symbol (list pred) increases-pred-str/ls decreases-pred-str/ls '()) ;; 4 symbol_pred
              (get-fda-tag
               (get-canonical-curie-from-synonyms-ls
               (flatten                 
                (get-rxnorm
                 (substitute
                  (flatten
                   (list (get-xrefs semmed-concept-key/xrefs subject-props-assoc)
                         (get-xrefs robokop-concept-key/eq-id subject-props-assoc)
                         (set->list (curie-synonyms subject-id)))                   
                   ) "#f" "NA") '()))))              
              (get-canonical-curie-from-synonyms-ls
               (query-hgnc-curie-for-hgnc-symbol
                (list (get-curie-synonyms-from-edge-subject/object-curie object-id)) '())) ;;5 linker_curie_nameO
              (get-curie-synonyms-from-edge-subject/object-curie object-id) ;; 6 linker_curie_nameO              
              subject-name
              subject-id
              subject-category
              pred               
              object-name
              object-id
              object-category
              (get-canonical-curie-from-synonyms-ls
               (flatten                 
                (get-rxnorm
                 (substitute
                  (flatten
                   (list (get-xrefs semmed-concept-key/xrefs subject-props-assoc)
                         (get-xrefs robokop-concept-key/eq-id subject-props-assoc)
                         (set->list (curie-synonyms subject-id)))                   
                   ) "#f" "NA") '())))
              (length (pubmed-ids-from-edge-props pred-props-assoc))              
              (string-join
               (map (lambda (pubmed)
                      (string-append PUBMED_URL_PREFIX (~a pubmed)))
                    (pubmed-ids-from-edge-props pred-props-assoc)) " ")                           
              ) '() "NA")
            els))])))))


#|

(str-converter
                 (flatten
                  (list 
                   (get-xrefs drug-bank/key drug-props-assoc))))
|#
#|
;;og
(define match-edge/with-S/O-gene-linker-ids
  (lambda (edges-ls els)
    (cond
      ((null? edges-ls) els)
      (else 
       (match (car edges-ls)                       
         [`(,db ,edge-cui
                (,subject-cui ,subject-id ,subject-name (,_ . ,subject-category) ,subject-props-assoc)
                (,object-cui ,object-id ,object-name (,_ . ,object-category) ,object-props-assoc)
                (,_ . ,pred) ,pred-props-assoc)
          (match-edge/with-S/O-gene-linker-ids           
           (cdr edges-ls)
           (cons
            (substitute
             (list              
              db
subject-name
              subject-id
              (get-curie-synonyms-from-edge-subject/object-curie subject-id) ;;3rd
              subject-category
              pred
              (append-predicate-symbol (list pred) increases-pred-str/ls decreases-pred-str/ls '())
              object-name
              object-id
              (get-curie-synonyms-from-edge-subject/object-curie object-id) ;;9th
              object-category
              (length (pubmed-ids-from-edge-props pred-props-assoc))
              (string-join
               (map (lambda (pubmed)
                      (string-append PUBMED_URL_PREFIX (~a pubmed)))
                    (pubmed-ids-from-edge-props pred-props-assoc)) " ")
              ) '() "NA")
            els))])))))

|#
#|
|#


#|
#;(get-canonical-curie-from-synonyms-ls
(filter/curie (set->list (curie-synonyms subject-id)) '() "HGNC:")) ;; subject 3rd
;;  (get-canonical-curie-from-synonyms-ls
(filter/curie (set->list (curie-synonyms object-id)) '() "HGNC:")) ;; object 9th
|#



(define 1-hop-gene-lookup
  (lambda (target-gene-ls els)
    (define 1-hop/query
      (lambda (target-gene)
        (printf "\nQUERY/GRAPH RUNNING ON:   ~a\n" target-gene)
        (cond
          ((null? target-gene)
           (cons "NO_QUERY_CONCEPT" els))
          (else
           (time (query/graph
               ((X #f)
                (TG target-gene))
               ((X->TG #f))
               (X X->TG TG)))))))
    (cond
      ((null? target-gene-ls) els)
      (else
       (1-hop-gene-lookup
        (cdr target-gene-ls)
        (cons
         (edges/query (1-hop/query (car target-gene-ls)) 'X->TG)
         els))))))

(define 1-hop-gene-lookup/allowable-predicates
  (lambda (target-gene-ls els predicates)
    (define 1-hop/query
      (lambda (target-gene)
        (printf "\nQUERY/GRAPH RUNNING ON:   ~a\n" target-gene)
        (time (query/graph
               ((X #f)
                (TG target-gene))
               ((X->TG predicates))
               (X X->TG TG)))))
    (cond
      ((null? target-gene-ls) els)
      (else
       (1-hop-gene-lookup/allowable-predicates
        (cdr target-gene-ls)
        (cons
         (edges/query (1-hop/query (car target-gene-ls)) 'X->TG)
         els)
        predicates)))))

(define 1-hop-drug-lookup/allowable-predicates/drug-->X
  (lambda (target-curie-ls els predicates)
    (define 1-hop/query
      (lambda (target-concept)
        (printf "\nQUERY/GRAPH RUNNING ON:   ~a\n" target-concept)
        (time (query/graph
               ((X #f)
                (TC target-concept))
               ((TC-->X predicates))
               (TC TC-->X X)))))
    (cond
      ((null? target-curie-ls) els)
      (else
       (1-hop-drug-lookup/allowable-predicates/drug-->X
        (cdr target-curie-ls)
        (cons
         (edges/query (1-hop/query (car target-curie-ls)) 'TC-->X)
         els
         )
        predicates)))))

#|
;;; here 
(define 1hop-gene-lookup/1-HOP-AFFECTOR-GENE--ALLp-->ALL-GENES
  (lambda (1hop-gene-ls els)
    (define 1-hop/query
      (lambda (1hop-gene)
        (printf "\nQUERY/GRAPH RUNNING ON:   ~a\n" target-gene)
        (time (query/graph
               ((X #f)
                (1H-GENE 1hop-gene))
               ((X->TG #f))
               (X X->TG TG)))))
    (cond
      ((null? target-gene-ls) els)
      (else
       (1-hop-gene-lookup
        (cdr target-gene-ls)
        (cons
         (edges/query (1-hop/query (car target-gene-ls)) 'X->TG)
         els))))))
|#
(define 2-hop-gene-lookup
  (lambda (target-gene-ls els)
    (define 1-hop/query
      (lambda (target-gene)
        (printf "\nQUERY/GRAPH RUNNING ON:   ~a\n" target-gene)
        (time (query/graph
               ((X #f)
                (TG target-gene))
               ((X->TG #f))
               (X X->TG TG)))))
    (let* ((1-hop-affector-genes/HGNC*
            (synonyms/query (1-hop/query (car target-gene-ls)) 'X))
           (1-hop-affector-genes/HGNC*
            (flatten
             (remove-item
              '()
              (map
               (lambda (ls) (filter/curie ls '() "HGNC:"))
               (map
                set->list
                1-hop-affector-genes/HGNC*))
              '()))))
      (printf "\n\n~a 1-HOP AFFECTOR GENE CONCEPTS FOUND!\n" (length 1-hop-affector-genes/HGNC*))
      (printf "\n\n~a 1-HOP AFFECTOR GENES HGNC-IDs FOUND!\n\n~a" (length 1-hop-affector-genes/HGNC*) 1-hop-affector-genes/HGNC*)
      (cond
        ((null? target-gene-ls) els)
        (else
         (displayln (format "\n\nPREPARING 1-HOP AFFECTOR GENES FOR 2-HOP QUERY!\n\n"))
         (let ((2-hop-affector-gene/edges (let loop ((1-hop-affector-genes/HGNC* 1-hop-affector-genes/HGNC*))
                                            (cond
                                              ((null? 1-hop-affector-genes/HGNC*) '())
                                              (else
                                               (cons
                                                (edges/query (1-hop/query (car 1-hop-affector-genes/HGNC*)) 'X->TG)
                                                (loop
                                                  (cdr 1-hop-affector-genes/HGNC*))))))))
           (append* 2-hop-affector-gene/edges els)))))))


(define 2-hop-gene-lookup/allowable-predicates
  (lambda (target-gene-ls els predicates)
    (define 1-hop/query
      (lambda (target-gene)
        (printf "\nQUERY/GRAPH RUNNING ON:   ~a\n" target-gene)
        (time (query/graph
               ((X #f)
                (TG target-gene))
               ((X->TG predicates))
               (X X->TG TG)))))
    (let* ((1-hop-affector-genes/HGNC*
            (synonyms/query (1-hop/query (car target-gene-ls)) 'X))
           (1-hop-affector-genes/HGNC*
            (flatten
             (remove-item
              '()
              (map
               (lambda (ls) (filter/curie ls '() "HGNC:"))
               (map
                set->list
                1-hop-affector-genes/HGNC*))
              '()))))
      (printf "\n\n~a 1-HOP AFFECTOR GENE CONCEPTS FOUND!\n" (length 1-hop-affector-genes/HGNC*))
      (printf "\n\n~a 1-HOP AFFECTOR GENES HGNC-IDs FOUND!\n\n~a" (length 1-hop-affector-genes/HGNC*) 1-hop-affector-genes/HGNC*)
      (cond
        ((null? target-gene-ls) els)
        (else
         (displayln (format "\n\nPREPARING 1-HOP AFFECTOR GENES FOR 2-HOP QUERY!\n\n"))
         (let ((2-hop-affector-gene/edges (let loop ((1-hop-affector-genes/HGNC* 1-hop-affector-genes/HGNC*))
                                            (cond
                                              ((null? 1-hop-affector-genes/HGNC*) '())
                                              (else
                                               (cons
                                                (edges/query (1-hop/query (car 1-hop-affector-genes/HGNC*)) 'X->TG)
                                                (loop
                                                  (cdr 1-hop-affector-genes/HGNC*))))))))
           (append* 2-hop-affector-gene/edges els)))))))



#|
(define gene-concept?
  (lambda (x)
    (or
     (boolean? (car x))
     (string-prefix? (car x) "HGNC:")
     (string-prefix? (car x) "ENSEMBL:")
     (string-prefix? (car x) "UniProtKB:")
     (string-prefix? (car x) "NCBIGene:")
     (string-prefix? (car x) "NCBIGENE:"))))
|#

(define gene-concept?
  (lambda (x)
    (or
     (string-prefix? x "HGNC:")
     (string-prefix? x "ENSEMBL:")
     (string-prefix? x "UniProtKB:")
     (string-prefix? x "NCBIGene:")
     (string-prefix? x "NCBIGENE:"))))

#|
(define drug-concept?
  (lambda (x)
    (or (boolean? (car x))
        (string-prefix? (car x) "CHEBI:")
        (string-prefix? (car x) "CHEMBL:")
        (string-prefix? (car x) "CHEMBL.")
        (string-prefix? (car x) "KEGG:")
        (string-prefix? (car x) "KEGG.")
        (string-prefix? (car x) "DRUGBANK:")
        (string-prefix? (car x) "RXNORM:"))))
|#

(define drug-concept?
  (lambda (x)
    (or (string-prefix? x "CHEBI:")
        (string-prefix? x "CHEMBL:")
        (string-prefix? x "CHEMBL.")
        (string-prefix? x "KEGG:")
        (string-prefix? x "KEGG.")
        (string-prefix? x "DRUGBANK:")
        (string-prefix? x "RXNORM:"))))

(define lat?
  (lambda (ls)
    (cond
      ((null? ls) #t)
      ((atom? (car ls))
       (lat? (cdr ls)))
      (else
       #f))))

(define get-export-edges
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((lat? (car ls))
       (cons (car ls)
             (get-export-edges (cdr ls))))
      (else
       (set-union (get-export-edges (car ls))
                  (get-export-edges (cdr ls)))))))



#|
(define affector-gene-edge?
  (lambda (ls)
    (match (car ls)
      [`(,db ,edge-cui
             (,subject-cui ,subject-id ,subject-name (,_ . ,subject-category) ,subject-props-assoc)
             (,object-cui ,object-id ,object-name (,_ . ,object-category) ,object-props-assoc)
             (,_ . ,pred)
             ,pred-props-assoc)
       (cond
         ((gene-concept? `(,subject-id))
          (list (car ls)))
         (else
          #f))])))
|#

(define filter-edges-by-affector-type
  (lambda (ls els-gene els-drug)
    (cond
      ((null? ls)
       (cons els-gene els-drug))
      (else
       (match (car ls)
      [`(,db ,edge-cui
             (,subject-cui ,subject-id ,subject-name (,_ . ,subject-category) ,subject-props-assoc)
             (,object-cui ,object-id ,object-name (,_ . ,object-category) ,object-props-assoc)
             (,_ . ,pred)
             ,pred-props-assoc)
       (cond
         ((ormap drug-concept? (set->list (curie-synonyms subject-id)))
          (filter-edges-by-affector-type
           (cdr ls)
           els-gene
           (cons (car ls) els-drug)))
         ((ormap gene-concept? (set->list (curie-synonyms subject-id)))
          (filter-edges-by-affector-type
           (cdr ls)
           (cons (car ls) els-gene)
           els-drug))
         (else
          (filter-edges-by-affector-type
           (cdr ls)
           els-gene
           els-drug)))])))))

(define get-subject-HGNC-from-edge 
  (lambda (edge)
    (car (filter/curie (set->list (curie-synonyms (list-ref edge 2))) '() "HGNC:"))))

#|MAKE 2HOP PATH|#
(define get-object-synonyms-from-edge
  (lambda (edge)
    (set->list (curie-synonyms (list-ref edge 7)))))
;;testing 
(define find-1-hop/2-hop-link
  (lambda (1-hop-edge 2-hop-edge-ls)
    (cond
      ((null? 2-hop-edge-ls) '())
      (else
       (let ((2-hop-edge (car 2-hop-edge-ls))
             (1-hop-subject-HGNC (list-ref 1-hop-edge 3)))
         (let ((2-hop-edge-object-synonyms (set->list (curie-synonyms (list-ref 2-hop-edge 7)))))                 
           (cond
             ((member? 1-hop-subject-HGNC 2-hop-edge-object-synonyms)
              (cons
               (list 2-hop-edge 1-hop-edge)
               (find-1-hop/2-hop-link
                1-hop-edge
                (cdr 2-hop-edge-ls))))
             (else
              (find-1-hop/2-hop-link
               1-hop-edge
               (cdr 2-hop-edge-ls))))))))))

;; export edges! (NOT BIOLINK EDGES)
(define make-2hop-path
  (lambda (1-hop-edge-ls 2-hop-edge-ls)
    (cond
      ((null? 1-hop-edge-ls) '())
      (else
       (cons
        (find-1-hop/2-hop-link (car 1-hop-edge-ls) 2-hop-edge-ls)
        (make-2hop-path (cdr 1-hop-edge-ls) 2-hop-edge-ls))))))

;;function takes list of 2hop paths and makes 2hop edges for export
;; arg here is a list of all paths for a particular gene synonym @ 1hop gene

;; a 2-hop-path
(define make-single-2hop-path-export
  (lambda (path)
    (cond
      ((and (pair? path)
            (lat? (car path))
            (lat? (cadr path)))
       (append (car path) (cadr path)))
      (else
       (error (format "EXPORT ERROR WITH EDGE: ~a\n" path))))))

(define column-headers/1hop
  '("edge_id"
    "db"
    "subject_linker_name"              
    "subject_linker_curie"
    "predicate_symbol"
    "fda_approved"
    "target_object_linker_name"
    "target_object_linker_curie"
    "subject_name"
    "subject_curie" 
    "subject_category"
    "predicate"              
    "target_object_name"
    "target_object_curie"
    "target_object_category"
    "rxnorm_drug_id"
    "pubmed_number"
    "pub_urls"                            
    ))

(define get-2hop-paths-for-single-1hop-affector-gene
  (lambda (all-2hop-paths-for-single-affector-gene)
    (cond
      ((null? all-2hop-paths-for-single-affector-gene) '())
      (else
       (cons
        (make-single-2hop-path-export (car all-2hop-paths-for-single-affector-gene))
        (get-2hop-paths-for-single-1hop-affector-gene (cdr all-2hop-paths-for-single-affector-gene)))))))  

(define make-2hop-path/export
  (lambda (2hop-paths-ls)
    (cond
      ((null? 2hop-paths-ls) '())
      (else
       (cons
        (get-2hop-paths-for-single-1hop-affector-gene (car 2hop-paths-ls))
        (make-2hop-path/export (cdr 2hop-paths-ls)))))))


(define start-function
  (lambda (gene-ls gene-ls-name)
    (cond
      ((null? gene-ls) (void))
      (else
       (handle-2hop-query (car gene-ls) gene-ls-name)
       (start-function (cdr gene-ls) gene-ls-name)))))

(define export-date
  (format "~a_~a_~a" 
          (number->string (date-month (seconds->date (current-seconds))))
          (number->string (date-day (seconds->date (current-seconds))))
          (number->string (date-year (seconds->date (current-seconds))))))

(define export-path
  "/Users/michaelpatton/git/automated_medikanren_queries/covid19/proviral_genes/")

(define export-path/antiviral
  "/Users/michaelpatton/git/automated_medikanren_queries/covid19/antiviral_gene_2hop/")

(define directory-path 
  (format "~a~a/" export-path export-date))
        
(define make-export-directory/proviral
  (if (directory-exists? directory-path)
      (error (format "CHECK FILE, IT MAY EXIST"))
      (make-directory directory-path)))

(define handle-2hop-query
  (time
   (lambda (curie gene-ls-name)
     (cond
       ((null? curie) (void))
       (else
        (let* ()
          
          (define curie-synonyms/CURIE
            (curie-synonyms/names curie))

          (define get-gene-curie-symbol-for-export
            (lambda (curie)
              (filter string?
                      (map (lambda (x)
                             (if (or (string-prefix? (car x) "HGNC:")
                                     (string-prefix? (car x) "CHEBI:"))
                                 (cdr x)
                                 #f))
                           (curie-synonyms/names curie)))))
          
          (define curie-str/EXPORT
            (car (get-gene-curie-symbol-for-export curie)))
                    
          (printf "\nBEGINNING 1-HOP LOOKUP ON CONCEPT: ~a aka ~a\n" curie curie-str/EXPORT)

          (printf "\n~a aka ~a SYNONYMS GATHERED FROM ALL KNOWLEDGE-GRAPHS:\n\n~a\n\n" curie curie-str/EXPORT curie-synonyms/CURIE)
          
          
          #|          
          (define 1-hop-affector/BIOLINK-edges
            (1-hop-gene-lookup (list curie) '()))
          |#

          #|
          ;;; DRUG-LOOKUP!
          (define 1-hop-affector-drug/BIOLINK-edges
            (1-hop-drug-lookup/allowable-predicates/drug-->X (list curie) '() allowable-predicates))

          
          
          ;; #### DRUG LOOKUP FILES
          (define drug--pred-->X/path
            (format "~a~a--ALLp-->X.tsv" directory-path gene-ls-name))
          (define drug--pred-->X/port
            (open-output-file drug--pred-->X/path #:exists 'append))

          (export-column-headers
           column-headers/1hop
           drug--pred-->X/port
           drug--pred-->X/path)

          (outer-loop
           1-hop-affector-drug/EXPORT-edges           
           drug--pred-->X/port)
        
          
          ;; #### DRUG LOOKUP FILES

          |#
          
          
          ;;GENE-LOOKUP!
          (printf "\n1-hop-affector/BIOLINK-edges\n")
          (define 1-hop-affector/BIOLINK-edges
            (1-hop-gene-lookup/allowable-predicates (list curie) '() allowable-predicates))

          
   
                             
          ;;trying predicate bumpers
          (printf "\n1-hop-affector-drug+gene/BIOLINK-edges\n")
          (define 1-hop-affector-drug+gene/BIOLINK-edges
            (remove-item '() (map (lambda (ls) (filter-edges-by-affector-type ls '() '())) 1-hop-affector/BIOLINK-edges) '()))
          
          (printf "\n1-hop-affector-gene/BIOLINK-edges\n")
          (define 1-hop-affector-gene/BIOLINK-edges
            (remove-item '() (map car 1-hop-affector-drug+gene/BIOLINK-edges) '()))

          
          (printf "\n1-hop-affector-drug/BIOLINK-edges\n")
          (define 1-hop-affector-drug/BIOLINK-edges
            (remove-item '() (map cdr 1-hop-affector-drug+gene/BIOLINK-edges) '()))
          
          (printf "\n1-hop-affector-gene/EXPORT-edges\n")
          (define 1-hop-affector-gene/EXPORT-edges
            (get-export-edges
             (remove-item
              '()
              (map (lambda (x) (match-edge/with-S/O-gene-linker-ids x '())) 1-hop-affector-gene/BIOLINK-edges)
              '())))
          
          
          
          (printf "\n1-hop-affector-drug/EXPORT-edges\n")
          (define 1-hop-affector-drug/EXPORT-edges
            (get-export-edges
             (remove-item
              '()
              (map (lambda (x) (match-edge/with-S/O-gene-linker-ids x '())) 1-hop-affector-drug/BIOLINK-edges)
              '())
             ))
          
          ;;(pretty-print (car 1-hop-affector-drug/EXPORT-edges))

          (printf "\n1-HOP LOOKUP FOR ~a COMPLETE!\n" curie-str/EXPORT)
  
          (define 1-hop-affector-gene/path
            (format "~a1HOP-AFFECTOR-GENES--ALLp-->~a.tsv" directory-path gene-ls-name))
          (define 1-hop-affector-drug/path
            (format "~a1HOP-AFFECTOR-DRUGS--ALLp-->~a.tsv" directory-path gene-ls-name))
        
          (define 1-hop-affector-gene/port
            (open-output-file 1-hop-affector-gene/path #:exists 'append))
          (define 1-hop-affector-drug/port
            (open-output-file 1-hop-affector-drug/path #:exists 'append))
                             
          
          (export-column-headers
           column-headers/1hop
           1-hop-affector-gene/port
           1-hop-affector-gene/path)

          (outer-loop
           1-hop-affector-gene/EXPORT-edges
           1-hop-affector-gene/port)
        
          (export-column-headers
           column-headers/1hop
           1-hop-affector-drug/port
           1-hop-affector-drug/path)

          (outer-loop
           1-hop-affector-drug/EXPORT-edges
           1-hop-affector-drug/port)
          
          (printf "\n1-HOP EXPORT COMPLETE:\nAFFECTOR-GENES-->ALL-PREDICATES-->~a\n" curie-str/EXPORT) 
          (printf "\n1-HOP EXPORT COMPLETE:\nAFFECTOR-DRUGS-->ALL-PREDICATES-->~a\n" curie-str/EXPORT)
                   
          #|START 2-HOP LOOKUP|#
          (printf "\nBEGINNING 2-HOP LOOKUP ON CONCEPT: ~a aka ~a\n" curie curie-str/EXPORT)
          
          #|
          (define 2-hop-affector/BIOLINK-edges
            (time
             (2-hop-gene-lookup (list curie) '())))
          |#
          #|
          ;; trying predicate bumpers
          (printf "\n2-hop-affector/BIOLINK-edges\n")
          (define 2-hop-affector/BIOLINK-edges
            (time
             (2-hop-gene-lookup/allowable-predicates (list curie) '() allowable-predicates)))
          
          (printf "\n2-hop-affector-drug+gene/BIOLINK-edges\n")
          (define 2-hop-affector-drug+gene/BIOLINK-edges
            (time
             (remove-item
              '()
              (map (lambda (ls) (filter-edges-by-affector-type ls '() '())) 2-hop-affector/BIOLINK-edges)
              '())
             ))      

          (printf "\n2-hop-affector-gene/BIOLINK-edges\n")
          (define 2-hop-affector-gene/BIOLINK-edges  
            (time (map car 2-hop-affector-drug+gene/BIOLINK-edges)))
          
          (printf "\n2-hop-affector-drug/BIOLINK-edges\n")
          (define 2-hop-affector-drug/BIOLINK-edges            
            (time
             (map cdr 2-hop-affector-drug+gene/BIOLINK-edges)))

          (printf "\n2-hop-affector-drug/EXPORT-edges\n")
          (define 2-hop-affector-drug/EXPORT-edges
            (time (get-export-edges
                   (remove-item
                    '()
                    (map (lambda (x) (match-edge/with-S/O-gene-linker-ids x '())) 2-hop-affector-drug/BIOLINK-edges)
                    '())
                   )))

                    
          (define 2-hop-affector-drug/path
            (format "~a2HOP-AFFECTOR-DRUGS--ALLp-->1HOP-AFFECTOR-GENES-->ALLp-->~a.tsv" directory-path gene-ls-name))
          (define 2-hop-affector-drug/port
            (open-output-file 2-hop-affector-drug/path #:exists 'append))

          (export-column-headers
           column-headers/1hop
           2-hop-affector-drug/port
           2-hop-affector-drug/path)

          (outer-loop
           2-hop-affector-drug/EXPORT-edges
           2-hop-affector-drug/port)

          
          #|
          ;; comment this out for now, don't need 2-hop affector genes until 3-hop affector drugs are computed
          (printf "\n2-hop-affector-gene/EXPORT-edges\n")
          (define 2-hop-affector-gene/EXPORT-edges
            (time (get-export-edges (map (lambda (x) (match-edge/with-S/O-gene-linker-ids x '())) 2-hop-affector-gene/BIOLINK-edges))))
          |#                    
          #|
          (define 2-hop-affector-drug-specific/path
            (format "~a2HOP-AFFECTOR-DRUGS--ALLp-->~a.tsv" directory-path curie-str/EXPORT))
          (define 2-hop-affector-drug-specific/port
            (open-output-file 2-hop-affector-drug-specific/path #:exists 'append))
          
          (export-column-headers
           column-headers/1hop
           2-hop-affector-drug-specific/port
           2-hop-affector-drug-specific/path)
          (outer-loop
           2-hop-affector-drug/EXPORT-edges
           2-hop-affector-drug-specific/port)
          
          (printf "\n2-HOP EXPORT COMPLETE:\nAFFECTOR-DRUGS--ALL-PREDICATES-->1-HOP-AFFECTOR GENE of ~a\n" curie-str/EXPORT)
          |#
          

           #|        
          (printf "\n\n2-HOP PATH:\n2-HOP-AFFECTOR-DRUG--ALL-PREDICATES-->1-HOP-AFFECTOR-GENE--ALL-PREDICATES-->~a\n\n" curie-str/EXPORT)
          |#
          #|
          (printf "\n2-hop-path/drug--predicate-->1hop-gene--predicate-->target-gene/EXPORT-edges\n")
          (define 2-hop-path/drug--predicate-->1hop-gene--predicate-->target-gene/EXPORT-edges
            (time
             (get-export-edges
              (make-2hop-path/export
               (make-2hop-path
                 1-hop-affector-gene/EXPORT-edges
                 2-hop-affector-drug/EXPORT-edges)))))
          
          (define 2-hop-affector-drug-->1hop-gene-->target-gene-paths/path
            (format "~a2HOP-AFFECTOR-DRUGS--ALLp-->1HOP-AFFECTOR-GENES--ALLp-->PROVIRAL-GENES.tsv" directory-path))
          (define 2-hop-affector-drug-->1hop-gene-->target-gene-paths/port
            (open-output-file 2-hop-affector-drug-->1hop-gene-->target-gene-paths/path #:exists 'append))

          (define column-headers/2hop
            '("edge_id"
              "db"
              "subject_linker_name"              
              "subject_linker_curie"
              "predicate_symbol"
              "fda_approved"
              "target_object_linker_name"
              "target_object_linker_curie"
              "subject_name"
              "subject_curie" 
              "subject_category"
              "predicate"              
              "target_object_name"
              "target_object_curie"
              "target_object_category"
              "rxnorm_drug_id"
              "pubmed_number"
              "pub_urls"
              "edge_id"
              "db"
              "subject_linker_name"              
              "subject_linker_curie"
              "predicate_symbol"
              "fda_approved"
              "target_object_linker_name"
              "target_object_linker_curie"
              "subject_name"
              "subject_curie" 
              "subject_category"
              "predicate"              
              "target_object_name"
              "target_object_curie"
              "target_object_category"
              "rxnorm_drug_id"
              "pubmed_number"
              "pub_urls"))

          (export-column-headers
           column-headers/2hop
           2-hop-affector-drug-->1hop-gene-->target-gene-paths/port
           2-hop-affector-drug-->1hop-gene-->target-gene-paths/path)

          (outer-loop
           2-hop-path/drug--predicate-->1hop-gene--predicate-->target-gene/EXPORT-edges
           2-hop-affector-drug-->1hop-gene-->target-gene-paths/port)
          
          (export-column-headers
           column-headers/2hop
           2-hop-affector-drug-->1hop-gene-->target-gene-paths-specific/port
           2-hop-affector-drug-->1hop-gene-->target-gene-paths-specific/path)

          (outer-loop
           2-hop-path/drug--predicate-->1hop-gene--predicate-->target-gene/EXPORT-edges
           2-hop-affector-drug-->1hop-gene-->target-gene-paths-specific/port)
          |#

          |#
          )
        
        (pretty-print "QUERY FINISHED!")

        )))))

(start-function all-hgncs "HUMAN-GENOME")


          #|

          (define 2-hop-affector-drug-->1hop-gene-->target-gene-paths-specific/path
          (format "~a2HOP-AFFECTOR-GENES--ALLp-->1HOP-AFFECTOR-GENES--ALLp-->~a.tsv" directory-path curie-str/EXPORT))
          (define 2-hop-affector-drug-->1hop-gene-->target-gene-paths-specific/port
            (open-output-file 2-hop-affector-drug-->1hop-gene-->target-gene-paths-specific/path  #:exists 'append))          

          
          (printf "\nBUILDING 2-HOP GENE PATHS FOR CONCEPT: ~a aka ~a\n\n" curie curie-str/EXPORT)
          (printf "\n\n2-HOP PATH:\n2-HOP-AFFECTOR-GENE--ALL-PREDICATES-->1-HOP-AFFECTOR-GENE--ALL-PREDICATES-->~a\n\n" curie-str/EXPORT)
          ;;test without remove-item
          (printf "\n2-hop-gene-path/EXPORT-edges\n")
          (define 2-hop-gene-path/EXPORT-edges
            (time
             (make-2hop-path
               1-hop-affector-gene/EXPORT-edges
               2-hop-affector-gene/EXPORT-edges)))

          (printf "\n2-hop-gene-paths-connected/EXPORT-edges\n")
          (define 2-hop-gene-paths-connected/EXPORT-edges
            (time
             (get-export-edges
             (make-2hop-path/export
              2-hop-gene-path/EXPORT-edges))))

          (define 2-hop-affector-gene-paths/path
            (format "~a2HOP-AFFECTOR-GENES--ALLp-->1HOP-AFFECTOR-GENES--ALLp-->PROVIRAL-GENES.tsv" directory-path))
          (define 2-hop-affector-gene-paths/port
            (open-output-file 2-hop-affector-gene-paths/path #:exists 'append))
          
          (define column-headers/2hop
            '("2hop_db"
              "2hop_subject_name"
              "2hop_subject_curie"
              "2hop_subject_canonical_curie"
              "2hop_subject_category"
              "2hop_predicate"
              "2hop_predicate_symbol"
              "2hop_object_name"
              "2hop_object_curie"
              "2hop_object_canonical_curie"
              "2hop_object_category"
              "2hop_pubmed_number"
              "2hop_pub_URLs"
              "1hop_db"
              "1hop_subject_name"
              "1hop_subject_curie"
              "1hop_subject_canonical_curie"
              "1hop_subject_category"
              "1hop_predicate"
              "1hop_predicate_symbol"
              "target_object_name"
              "target_object_curie"
              "target_object_canonical_curie"
              "1hop_target_object_category"
              "1hop_pubmed_number"
              "1hop_pubmed_URLs"))

          (export-column-headers
           column-headers/2hop
           2-hop-affector-gene-paths/port
           2-hop-affector-gene-paths/path)

          (outer-loop
           2-hop-gene-paths-connected/EXPORT-edges
           2-hop-affector-gene-paths/port)
          
          (define 2-hop-affector-gene-paths-specific/path
          (format "~a2HOP-AFFECTOR-GENES--ALLp-->1HOP-AFFECTOR-GENES--ALLp-->~a.tsv" directory-path curie-str/EXPORT))
          (define 2-hop-affector-gene-paths-specific/port
            (open-output-file 2-hop-affector-gene-paths-specific/path #:exists 'append))
          
          (export-column-headers
           column-headers/2hop
           2-hop-affector-gene-paths-specific/port
           2-hop-affector-gene-paths-specific/path)

          (outer-loop
           2-hop-gene-paths-connected/EXPORT-edges
           2-hop-affector-gene-paths-specific/port)
          |#
