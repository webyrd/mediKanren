#lang racket
(provide (all-defined-out))
(require "query.rkt"
         racket/engine)

#|proviral gene list for 2-hop|#

;; hgnc:12529
#|
(define pro-viral-gene-list
  '("HGNC:13557"))
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

(define prune-xrefs
  (lambda (ls els)
    (cond
      ((null? ls) els)
      ((or (void? (car ls))
           (boolean? (car ls)))
       (prune-xrefs (cdr ls) els))
      ((string-contains? (car ls) "HGNC")
       (cons (car ls)
             (prune-xrefs (cdr ls)
                          (cons
                           (string-replace (car ls) "HGNC:" "HGNC:HGNC:") els))))
      ((string-contains? (car ls) "NCBI")
       (prune-xrefs (cdr ls)
                    (cons 
                     (string-replace (car ls) "NCBIGENE:" "NCBIGene:") els)))
      (else
       (prune-xrefs (cdr ls) (cons (car ls) els)))))) 

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
    "decreases_synthesis_of"
    "decreases_transport_of"
    "decreases_uptake_of"
    "prevents"
    "treats"
    "disrupts"
    "inhibits"
    "increases_degradation_of"
    "decreases_activity_of"
    "decreases_expression_of"))

(define increases-pred-str/ls
  '("positively_regulates"
     "produces"
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
     "stimulates"
     "activator"))

(define predicate-str/ls
  '("physically_interacts_with"
    "regulates"
    "directly_interacts_with"
    "regulates_expression_of")) 

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

;;get simple pieces of edges
(define edge-matcher/SPO
  (lambda (ls els)
    (cond
      ((null? ls) els)
      (else
       (match (car ls)
         [`(,db ,edge-cui
               (,subject-cui ,subject-id ,subject-name (,_ . ,subject-category) ,subject-props-assoc)
               (,object-cui ,object-id ,object-name (,_ . ,object-category) ,object-props-assoc)
               (,_ . ,pred)
               ,pred-props-assoc)
          (edge-matcher/SPO
           (cdr ls)
           (set-union
            (list `((,db) (,subject-name . ,subject-id) ,pred (,object-name . ,object-id))) 
            els))])))))

(define gene-filter
  (lambda (ls els)
    (cond
      ((null? ls) (set-union els))
      ((or (string-prefix? (car ls) "HGNC:")
           (string-prefix? (car ls) "ENSEMBL:")
           (string-prefix? (car ls) "UniProtKB:")
           (string-prefix? (car ls) "NCBIGene:")
           (string-prefix? (car ls) "NCBIGENE:"))
       (gene-filter
        (cdr ls)
        (cons (car ls) els)))
      (else
       (gene-filter (cdr ls) els)))))

(define drug-filter
  (lambda (ls els)
    (cond
      ((null? ls) (set-union els))
      ((or (string-prefix? (car ls) "CHEBI:")
           (string-prefix? (car ls) "CHEMBL:")
           (string-prefix? (car ls) "CHEMBL.")
           (string-prefix? (car ls) "KEGG:")
           (string-prefix? (car ls) "KEGG.")
           (string-prefix? (car ls) "DRUGBANK:")
           (string-prefix? (car ls) "RXNORM:"))
       (drug-filter
        (cdr ls)
        (cons (car ls) els)))
      (else
       (drug-filter (cdr ls) els)))))

(define filter/curie
  (lambda (ls els curie)
    (cond
      ((null? ls) (set-union els))
      ((string-prefix? (car ls) curie)
       (filter/curie
        (cdr ls)
        (cons (car ls) els) curie))
      (else
       (filter/curie (cdr ls) els curie)))))

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

(define match-edge/for-export
  (lambda (edges-ls els)
    (cond
      ((null? edges-ls) els)
      (else 
       (match (car edges-ls)         
         [`(,db ,edge-cui (,subject-cui ,subject-id ,subject-name (,_ . ,subject-category) ,subject-props-assoc)
                (,object-cui ,object-id ,object-name (,_ . ,object-category) ,object-props-assoc)
                (,_ . ,pred) ,pred-props-assoc)
          (match-edge/for-export
           (cdr edges-ls)
            (cons 
            (substitute
             (list
              db
              subject-name
              subject-id
              subject-category
              pred
              (append-predicate-symbol (list pred) increases-pred-str/ls decreases-pred-str/ls '())
              object-name
              object-id
              object-category
              (length (pubmed-ids-from-edge-props pred-props-assoc))
              (string-join
               (map (lambda (pubmed)
                      (string-append PUBMED_URL_PREFIX (~a pubmed)))
                    (pubmed-ids-from-edge-props pred-props-assoc)) " ")
              ) '() "NA") els))]
         [`((,c . ,c*))
          (match-edge/for-export (cdr edges-ls) els)]
         [`()
          (match-edge/for-export (cdr edges-ls) els)])))))
#|
(define match-edge/for-export
  (lambda (edges-ls els)
    (cond
      ((null? edges-ls) els)
      (else 
       (match (car edges-ls)         
         [`(,db ,edge-cui (,subject-cui ,subject-id ,subject-name (,_ . ,subject-category) ,subject-props-assoc)
                (,object-cui ,object-id ,object-name (,_ . ,object-category) ,object-props-assoc)
                (,_ . ,pred) ,pred-props-assoc)
          (match-edge/for-export
           (cdr edges-ls)
            (cons 
            (substitute
             (list
              db
              subject-name
              subject-id
              subject-category
              pred
              (append-predicate-symbol (list pred) increases-pred-str/ls decreases-pred-str/ls '())
              object-name
              object-id
              object-category
              (length (pubmed-ids-from-edge-props pred-props-assoc))
              (string-join
               (map (lambda (pubmed)
                      (string-append PUBMED_URL_PREFIX (~a pubmed)))
                    (pubmed-ids-from-edge-props pred-props-assoc)) " ")
              ) '() "NA") els))]
         [`((,c . ,c*))
          (match-edge/for-export (cdr edges-ls) els)]
         [`()
          (match-edge/for-export (cdr edges-ls) els)])))))
|#

(define 1-hop-gene-lookup
  (lambda (target-gene-ls els)
    (define 1-hop/query
      (lambda (target-gene)
        (printf "\nQUERY/GRAPH RUNNING ON:   ~a\n" target-gene)
        (time (query/graph
               ((X #f)
                (TG target-gene))
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


;; [X] --ALL-PREDICATES--> [1-HOP-AFFECTOR GENE] --ALL-PREDICATES--> TARGET-GENE

;; list of 47 lists, each with the 2-hop edges
;; [X] --ALL-PREDICATES--> [1-HOP-AFFECTOR GENE] 
;; need to filter by X


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
    (or 
        (string-prefix? x "CHEBI:")
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

(define start-function
  (lambda (gene-ls)
    (cond
      ((null? gene-ls) (void))
      (else
       (handle-2hop-query (car gene-ls))
       (start-function (cdr gene-ls))))))

(define export-date
  (format "~a_~a_~a" 
          (number->string (date-month (seconds->date (current-seconds))))
          (number->string (date-day (seconds->date (current-seconds))))
          (number->string (date-year (seconds->date (current-seconds))))))
(define export-path/proviral
  "/Users/michaelpatton/git/automated_medikanren_queries/covid19/proviral_gene_2hop/")

(define export-path/antiviral
  "/Users/michaelpatton/git/automated_medikanren_queries/covid19/antiviral_gene_2hop/")

(define directory-path/proviral 
  (format "~a~a/" export-path/proviral export-date))
        
(define make-export-directory/proviral
  (if (directory-exists? directory-path/proviral)
      (error (format "CHECK FILE, IT MAY EXIST"))
      (make-directory directory-path/proviral)))

(define handle-2hop-query
  (time
   (lambda (curie-ls)
     (cond
       ((null? curie-ls) (void))
       (else
        (let* ()
          ;; 1-hop output files
          (define 1-hop-affector-gene/path
            (format "~a1HOP-AFFECTOR-GENES--ALLp-->PROVIRAL-GENES.tsv" directory-path/proviral))
          (define 1-hop-affector-drug/path
            (format "~a1HOP-AFFECTOR-DRUGS--ALLp-->PROVIRAL-GENES.tsv" directory-path/proviral))
          (define 1-hop-affector-gene/port
            (open-output-file 1-hop-affector-gene/path #:exists 'append))
          (define 1-hop-affector-drug/port
            (open-output-file 1-hop-affector-drug/path #:exists 'append))
          ;; 2-hop output files 
          (define 2-hop-affector-gene/path
            (format "~a2HOP-AFFECTOR-GENES--ALLp-->PROVIRAL-GENES.tsv" directory-path/proviral))
          (define 2-hop-affector-drug/path
            (format "~a2HOP-AFFECTOR-DRUGS--ALLp-->PROVIRAL-GENES.tsv" directory-path/proviral))
          (define 2-hop-affector-gene/port
            (open-output-file 2-hop-affector-gene/path #:exists 'append))
          (define 2-hop-affector-drug/port
            (open-output-file 2-hop-affector-drug/path #:exists 'append))
          ;;2-hop complete edge for graph-visualization
          (define 2-hop-affector-gene-for-data-visualization/path
            (format "~a2HOP-AFFECTOR-GENE--ALLp-->1HOP-AFFECTOR-GENE--ALLp-->TARGET-PROVIRAL-GENES.tsv" directory-path/proviral))
          (define 2-hop-affector-gene-for-data-visualization/port
            (open-output-file 2-hop-affector-gene-for-data-visualization/path #:exists 'append))
          
          
          (define column-headers
            '("db"
            "subject_name"
            "subject_curie"
            "subject_category"
            "predicate"
            "predicate_symbol"
            "object"
            "object_curie"
            "object_category"
            "pubmed_number"
            "pub_URLs"))

          
        (printf "BEGINNING 1-HOP LOOKUP\n")
        
        (define 1-hop-affector-edges
          (1-hop-gene-lookup (list curie-ls) '()))

        (define drug/gene-1-hop-affector/filtered
          (map (lambda (ls)
                 (filter-edges-by-affector-type
                  ls '() '()))
               1-hop-affector-edges))

        (define 1-hop-affector-gene-edges/filtered 
          (remove-item '() (map car drug/gene-1-hop-affector/filtered) '()))

        #|
        (printf "1-HOP-AFFECTOR-GENE-EDGES/FILTERED\n")
        (pretty-print 1-hop-affector-gene-edges/filtered)
        |#
        
        (define 1-hop-affector-drug-edges/filtered
          (remove-item '() (map cdr drug/gene-1-hop-affector/filtered) '()))

        (define 1-hop-affector-gene-edges/filtered-grouped
          (map (lambda (x) (match-edge/for-export x '())) 1-hop-affector-gene-edges/filtered))

        #|
        (printf "1-HOP-AFFECTOR-GENE-EDGES/FILTERED-GROUPED\n")
        (pretty-print 1-hop-affector-gene-edges/filtered-grouped)
        |#
                
        (define 1-hop-affector-gene-edges/filtered-export
          (get-export-edges (map (lambda (x) (match-edge/for-export x '())) 1-hop-affector-gene-edges/filtered)))

        

        ;; get Predicate name of 1hop S--P-->O query
        (define 1-hop/predicate-name
          (list
           (map (lambda (x) (list-ref x 4)) 1-hop-affector-gene-edges/filtered-export)
           (map (lambda (x) (list-ref x 5)) 1-hop-affector-gene-edges/filtered-export)))
         
        ;; get Object name of 1hop S--P-->O query
        (define 1-hop/object-name
          (list
           (map (lambda (x) (list-ref x 6)) 1-hop-affector-gene-edges/filtered-export)
           (map (lambda (x) (list-ref x 7)) 1-hop-affector-gene-edges/filtered-export))) 
        #|
        (printf "1-HOP-AFFECTOR-GENE-EDGES/FILTERED-EXPORT\n")
        (pretty-print 1-hop-affector-gene-edges/filtered-export)
        |#
        
        (define 1-hop-affector-drug-edges/filtered-export
          (get-export-edges (map (lambda (x) (match-edge/for-export x '())) 1-hop-affector-drug-edges/filtered)))
        
        (export-column-headers
         column-headers
         1-hop-affector-gene/port
         1-hop-affector-gene/path)
        
        (outer-loop
         1-hop-affector-gene-edges/filtered-export
         1-hop-affector-gene/port)
        
        (export-column-headers
         column-headers
         1-hop-affector-drug/port
         1-hop-affector-drug/path)
        
        (outer-loop
         1-hop-affector-drug-edges/filtered-export
         1-hop-affector-drug/port)

        
        #|START 2-HOP LOOKUP|#
        (printf "BEGINNING 2-HOP LOOKUP\n")
        
        
        (define 2-hop-affector-edges
            (2-hop-gene-lookup (list curie-ls) '()))
          ;; x lists, each split into two lists, genes and drugs affectors
          
        (define drug/gene-2-hop-affector/filtered
            (map (lambda (ls)
                   (filter-edges-by-affector-type
                    ls '() '()))
                 2-hop-affector-edges))


        (define 2-hop-affector-gene-edges/filtered 
          (remove-item '() (map car drug/gene-2-hop-affector/filtered) '()))
        
        (define 2-hop-affector-drug-edges/filtered
          (remove-item '() (map cdr drug/gene-2-hop-affector/filtered) '()))

        #|
        (map (lambda (x) (edge-matcher/SPO x '())) 2-hop-affector-gene-edges/filtered)
        (map (lambda (x) (edge-matcher/SPO x '())) 2-hop-affector-drug-edges/filtered)
        |#

        (define 2-hop-affector-gene-edges/filtered-export
          (get-export-edges (map (lambda (x) (match-edge/for-export x '())) 2-hop-affector-gene-edges/filtered)))
        ;; x lists of drugs ready for export
        
        ;;(pretty-print 2-hop-affector-gene-edges/filtered-export)
        
        (define 2-hop-affector-drug-edges/filtered-export
          (get-export-edges (map (lambda (x) (match-edge/for-export x '())) 2-hop-affector-drug-edges/filtered)))
        
        (export-column-headers
         column-headers
         2-hop-affector-gene/port
         2-hop-affector-gene/path)
        
        (outer-loop
         2-hop-affector-gene-edges/filtered-export
         2-hop-affector-gene/port)
        
        (export-column-headers
         column-headers
         2-hop-affector-drug/port
         2-hop-affector-drug/path)
        
        (outer-loop
         2-hop-affector-drug-edges/filtered-export
         2-hop-affector-drug/port)

          
          (define column-headers/data-visualization-full
            '("2hop_db"
              "2hop_subject_name"
              "2hop_subject_curie"
              "2hop_subject_category"
              "2hop_predicate"
              "2hop_predicate_symbol"
              "2hop_object"
              "2hop_object_curie"
              "2hop_object_category"
              "2hop_pubmed_number"
              "2hop_pub_URLs"
              "1hop_db"
              "1hop_subject_name"
              "1hop_subject_curie"
              "1hop_subject_category"
              "1hop_predicate"
              "1hop_predicate_symbol"
              "1hop_target_object"
              "1hop_target_object_curie"
              "1hop_target_object_category"
              "1hop_pubmed_number"
              "1hop_pubmed_URLs"))

         #|BEGIN APPENDING 2-hop/1-hop edges together|#
          ;; get subject name of 1hop S--P-->O query
          ;; get object name of 2hop S--P-->O query
          ;; if curie synonyms from the object of the 2hop query & subject of 1hop query
          ;; append the edges together 
          (define 1-hop/subject-curie
            (map (lambda (x) (list-ref x 2)) 1-hop-affector-gene-edges/filtered-export))

          (define 1-hop/subject-curie-synonyms
            (map curie-synonyms (map (lambda (x) (list-ref x 2)) 1-hop-affector-gene-edges/filtered-export)))
          
          (define 2-hop/object-curie
            (map (lambda (x) (list-ref x 7)) 2-hop-affector-gene-edges/filtered-export))

          (define 2-hop/object-curie-synonyms
            (map curie-synonyms (map (lambda (x) (list-ref x 7)) 2-hop-affector-gene-edges/filtered-export)))
          
          ;;takes filtered-export edges
          
          (define 1-hop/2-hop-edge-appender
            (lambda (1-hop-ls 2-hop-ls els)
              (cond
                ((null? 1-hop-ls) els)
                (else
                 (let ((2-hop-paths
                        (let ((1-hop-subject-HGNC (car (filter/curie (set->list (curie-synonyms (list-ref (car 1-hop-ls) 2))) '() "HGNC:")))
                              (2-hop-object-synonyms (set->list (curie-synonyms (list-ref (car 2-hop-ls) 7))))
                              (els-loop '()))
                          (cond
                            ((member? 1-hop-subject-HGNC 2-hop-object-synonyms)
                             (1-hop/2-hop-edge-appender
                              (cdr 1-hop-ls)
                              2-hop-ls
                              (cons
                               (list (car 2-hop-ls)
                                     (car 1-hop-ls))
                                els-loop)))
                           (else
                            (1-hop/2-hop-edge-appender
                             (cdr 1-hop-ls)
                             2-hop-ls
                             els-loop))))))
                   (set-union 2-hop-paths els))))))
          
       (pretty-print "QUERY FINISHED!")
        
          ))))))

 
;;(start-function pro-viral-gene-list)

;;(handle-2hop-query pro-viral-gene-list) 

 
;;(start-function anti-viral-gene-list)


(define 1-hop/edges
  '((semmed
     "ACE2 (human)"
     "HGNC:13557"
     "gene"
     "coexists_with"
     "0"
     "OFC3 gene"
     "UMLS:C1417942"
     "gene"
     2
     "https://www.ncbi.nlm.nih.gov/pubmed/17534374 https://www.ncbi.nlm.nih.gov/pubmed/17534374")
    (rtx
     "ACE2 Gene"
     "NCIT:C102527"
     "http://w3id.org/biolink/vocab/Gene"
     "equivalent_to"
     "0"
     "#f"
     "HGNC:8122"
     "http://w3id.org/biolink/vocab/Gene"
     0
     "")))

(define 2-hop/edges
  '((rtx
     "CYSRT1"
     "UniProtKB:A8MQ03"
     "http://w3id.org/biolink/vocab/Protein"
     "physically_interacts_with"
     "0"
     "ACE2 Gene"
     "NCIT:C102527"
     "http://w3id.org/biolink/vocab/Protein"
     0
     "")))





          #|
          (define gene-curie-symbol-for-export
            (car (filter string? (map (lambda (x) (if (string-prefix? (car x) "HGNC:") (cdr x) #f)) (curie-synonyms/names curie-ls)))))
          |#

          #|
          (define directory-path/proviral 
            (format "~a~a~a/" export-path/proviral export-date gene-curie-symbol-for-export))
          |#
  #|        
           ;; one file
           (define directory-path/proviral 
             (format "~a~a/" export-path/proviral export-date))
|#
          #|
          (define directory-path/antiviral ;
          (format "~a~a~a/" export-path/antiviral export-date gene-curie-symbol-for-export)) ;
                                        ;
          |#
#|
          
          (define make-export-directory/proviral
            (if (directory-exists? directory-path/proviral)
                (error (format "CHECK FILE, IT MAY EXIST"))
                (make-directory directory-path/proviral)))
|#
          #|
          (define make-export-directory/antiviral ;
          (if (directory-exists? directory/path) ;          (error (format "CHECK FILE, IT MAY EXIST")) ;
          (make-directory directory-path/antiviral))) ;
          |#
        

#|

(define 2-hop-affector-edges
  (2-hop-gene-lookup '("HGNC:8122") '()))
    ;; x lists, each split into two lists, genes and drugs affectors 


(define drug/gene-2-hop-affector/filtered
  (map (lambda (ls)
         (filter-edges-by-affector-type
          ls '() '()))
       2-hop-affector-edges))


(define 2-hop-affector-gene-edges/filtered 
  (remove-item '() (map car drug/gene-2-hop-affector/filtered) '()))

(define 2-hop-affector-drug-edges/filtered
(remove-item '() (map cdr drug/gene-2-hop-affector/filtered) '()))



#|
(map (lambda (x) (edge-matcher/SPO x '())) 2-hop-affector-gene-edges/filtered)
(map (lambda (x) (edge-matcher/SPO x '())) 2-hop-affector-drug-edges/filtered)
|#

(define 2-hop-affector-gene-edges/filtered-export
  (map (lambda (x) (match-edge/for-export x '())) 2-hop-affector-gene-edges/filtered))
    ;; x lists of drugs ready for export

(define 2-hop-affector-drug-edges/filtered-export
  (map (lambda (x) (match-edge/for-export x '())) 2-hop-affector-drug-edges/filtered))
|#

#|
(define make-2-hop-affector-gene/drug-edges
  (lambda (curie-ls els-proviral-gene els-proviral-drug)
    (define 2-hop-affector-edges
      (2-hop-gene-lookup curie-ls '()))
    ;; x lists, each split into two lists, genes and drugs affectors 
    (define drug/gene-2-hop-affector/filtered
      (map (lambda (ls)
             (filter-edges-by-affector-type
              ls '() '()))
           2-hop-affector-edges))
    ;; x lists of genes ready for export
    (define 2-hop-affector-gene-edges/filtered 
      (remove-item '() (map car drug/gene-2-hop-affector/filtered) '()))
    (define 2-hop-affector-drug-edges/filtered
      (remove-item '() (map cdr drug/gene-2-hop-affector/filtered) '()))
    (define 2-hop-affector-gene-edges/filtered-export
      (remove-item '() (map (lambda (x) (match-edge/for-export x '())) 2-hop-affector-gene-edges/filtered) '()))
    ;; x lists of drugs ready for export
    (define 2-hop-affector-drug-edges/filtered-export
      (remove-item '() (map (lambda (x) (match-edge/for-export x '())) 2-hop-affector-drug-edges/filtered) '()))     
    (cond
      ((null? curie-ls)
       (cons els-proviral-gene els-proviral-drug))
      (else
       (make-2-hop-affector-gene/drug-edges
        (cdr curie-ls)
        (cons 2-hop-affector-gene-edges/filtered-export els-proviral-gene)
        (cons 2-hop-affector-drug-edges/filtered-export els-proviral-drug)
        )))))
|#
#|
(define 2-hop-affector-gene/drug-edges/filtered/proviral-genes
  (make-2-hop-affector-gene/drug-edges '("HGNC:8122") '() '()))
|#


;; HGNC:8122 only has 2 gene regulators 
#|

(define 2-hop-affector-gene/drug-edges/filtered/proviral-genes
  (make-2-hop-affector-gene/drug-edges '("HGNC:8122") '() '()))
 
(define 2-hop-affector-gene-edges/filtered/proviral-genes
  (map car 2-hop-affector-gene/drug-edges/filtered/proviral-genes))

(define 2-hop-affector-drug-edges/filtered/proviral-genes
  (map cdr 2-hop-affector-gene/drug-edges/filtered/proviral-genes))

|#


#|
(define export-date
  (format "~a_~a_~a" 
          (number->string (date-month (seconds->date (current-seconds))))
          (number->string (date-day (seconds->date (current-seconds))))
          (number->string (date-year (seconds->date (current-seconds))))))

(define export-path/proviral
  "/Users/michaelpatton/git/automated_medikanren_queries/covid19/proviral_gene_2hop/")

(define export-path/antiviral
  "/Users/michaelpatton/git/automated_medikanren_queries/covid19/antiviral_gene_2hop/")

(define directory-path/proviral 
  (format "~a~a/" export-path/proviral export-date))

(define make-export-directory/proviral
  (if (directory-exists? directory-path/proviral)
      (error (format "CHECK FILE, IT MAY EXIST"))
      (make-directory directory-path/proviral)))

(define 2-hop-affector-gene/path
  (format			
   "~a_2HOP-AFFECTOR-GENES--ALLp-->PROVIRAL-GENES.tsv"
   directory-path/proviral))

(define 2-hop-affector-drug/path
  (format			
   "~a_2HOP-AFFECTOR-DRUGS--ALLp-->PROVIRAL-GENES.tsv"
   directory-path/proviral))

(define 2-hop-affector-gene/port
  (open-output-file 2-hop-affector-gene/path #:exists 'can-update))

(define 2-hop-affector-drug/port
  (open-output-file 2-hop-affector-drug/path #:exists 'can-update))

(define column-headers
  '("db"
    "subject name"
    "subject curie"
    "subject category"
    "predicate"
    "predicate symbol"
    "object"
    "object curie"
    "object category"
    "pubmed number"
    "pub URLs"))

(export-column-headers
 column-headers
 2-hop-affector-gene/port
 2-hop-affector-gene/path)

(outer-loop
 2-hop-affector-gene-edges/filtered-export
 2-hop-affector-gene/port)

(export-column-headers
 column-headers
 2-hop-affector-drug/port
 2-hop-affector-drug/path)

(outer-loop
 2-hop-affector-drug-edges/filtered-export
 2-hop-affector-drug/port)


|#



#|
(define 2-hop-affector-edges/NGLY1
  (2-hop-gene-lookup '("HGNC:21625") '()))
|#
#|
;; 47 lists, each split into two lists, genes and drugs affectors 
(define drug/gene-2-hop-affector/NGLY1/filtered
  (map (lambda (ls)
         (filter-edges-by-affector-type
          ls '() '()))
       2-hop-affector-edges/NGLY1))

(define 2-hop-affector-gene-edges/filtered 
  (map car drug/gene-2-hop-affector/NGLY1/filtered))

(define 2-hop-affector-drug-edges/filtered
  (map cdr drug/gene-2-hop-affector/NGLY1/filtered))

;; 47 lists of genes ready for export
(define 2-hop-affector-gene-edges/filtered-export
  (map (lambda (x) (match-edge/for-export x '())) 2-hop-affector-gene-edges/filtered))

;; 47 lists of drugs ready for export
(define 2-hop-affector-drugs-edges/filtered-export
  (map (lambda (x) (match-edge/for-export x '())) 2-hop-affector-drug-edges/filtered))
|#


#|
(define date
  (seconds->date (current-seconds)))

(define export-date
  (format "~a_~a_~a" 
          (number->string (date-month (seconds->date (current-seconds))))
          (number->string (date-day (seconds->date (current-seconds))))
          (number->string (date-year (seconds->date (current-seconds))))))

(define directory-path/proviral
  (format "~a~a/" export-path/proviral export-date))
(define directory-path/antiviral
  (format "~a~a/" export-path/proviral export-date))

(define make-export-directory/proviral
            (if (directory-exists? directory/path)
                (error (format "CHECK FILE, IT MAY EXIST"))
                (make-directory directory/path)))

(define make-export-directory/antiviral
            (if (directory-exists? directory/path)
                (error (format "CHECK FILE, IT MAY EXIST"))
                (make-directory directory/path)))
|#





;; we need a function to inspect each edge in a list
;; and determine if its subject is: A: Drug B: Gene
#|
(define 2-hop-affector-gene/drugs-edges/NGLY1
  (map (lambda (ls) (filter-edges-by-affector-type ls '() '())) 2-hop-affector-edges/NGLY1))
|#
#|
(define 2-hop-affector-drugs-edges/NGLY1
  (map (lambda (ls) (filter-edges-by-affector-type ls '() '())) 2-hop-affector-edges/NGLY1))

(define 2-hop-affector-gene-edges/NGL1Y
  (map (lambda (ls) (affector-gene-edge? ls)) 2-hop-affector-edges/NGLY1))

(define 2-hop-affector-gene-edges/NGLY1
  (map (lambda (x) (filter affector-gene-edge? x)) 2-hop-affector-edges/NGLY1))

(define 2-hop-affector-drug-edges/NGLY1
  (map (lambda (x) (filter affector-drug-edge? x)) 2-hop-affector-edges/NGLY1))
|#

#|
(map concept->name (map (lambda (ls) (edge->subject ls)) 2-hop-affector-edges/NGLY1))

(map (lambda (ls) (edge->subject ls)) 2-hop-affector-edges/NGLY1)


;; gives names of all subjects in edge list 
(map concept->name (map edge->subject 2-hop-affector-edges/NGLY1))
|#

#|
;; gives all preds
(map edge->pred (car 2-hop-affector-edges/NGLY1))

;; gives all concept names 
(map concept->name (map edge->object (car 2-hop-affector-genes/NGLY1)))
|#


#|

(define NGLY1
  (time (query/graph
         ((X       #f)
(NGLY1 "HGNC:17646"))
         ((X->NGLY1 #f))
         (X X->NGLY1 NGLY1))))
|#

#|
(define NGLY1
  (time (query/graph
         ((X       #f)
          (NGLY1 "HGNC:17646"))
         ((X->NGLY1 #f))
         (X X->NGLY1 NGLY1))))
(define NGLY1/synonyms (curie-synonymsp/names "HGNC:17646"))

(define X->NGLY1/simple
  (edge-matcher X->NGLY1 '()))

(define 1-hop/concepts->NGLY1 (curies/query NGLY1 'X))

(define X->NGLY1 (edges/query NGLY1 'X->NGLY1))

;; dont have to map curie-synonyms 
(define 1-hop-affector-genes/NGLY1
  (remove-item
   '()
   (map (lambda (ls) (gene-filter ls '()))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->NGLY1))) '()))

;; use filter 
(define 1-hop-affector-genes-HGNC/NGLY1
  (remove-item
   '()
   (map (lambda (ls) (filter/curie ls '() "HGNC:"))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->NGLY1))) '()))

(define 1-hop-affector-genes-names/NGLY1
  (map (lambda (ls) (curie-synonyms/names ls))
                      (flatten 1-hop-affector-genes-HGNC/NGLY1)))

(define 1-hop-affector-drugs/NGLY1
  (remove-item
   '()
   (map (lambda (ls) (drug-filter ls '()))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->NGLY1))) '()))

(define 1-hop-affector-drugs-names/NGLY1
  (map (lambda (ls) (curie-synonyms/names ls))
                      (flatten 1-hop-affector-drugs/NGLY1)))

(define 1-hop-affector-drugs-DRUGBANK/NGLY1 
  (remove-item
   '()
   (map (lambda (ls) (filter/curie ls '() "DRUGBANK:"))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->NGLY1))) '()))

(define X->NGLY1/preds
  (remove-duplicates (map (lambda (ls) (list-ref ls 2)) X->NGLY1/simple)))

;; use NGYL1 gene, much fewer edges

|#

#|
,en synonymize.rkt
,en query.rkt
|#
#|
;; 670 edges
(define ACE2 (time (query/graph
((X       #f)
(ACE2 "HGNC:13557"))
((X->ACE2 #f))
(X X->ACE2 ACE2))))

;; all X's in the X--pred-->ACE2 edges
(define 1-hop/concepts->ACE2 (curies/query ACE2 'X))

;; gives full edge list X--pred-->ACE2
(define X->ACE2 (edges/query ACE2 'X->ACE2))

;; gives db S P O of all edges 
(define X->ACE2/simple
  (edge-matcher X->ACE2 '()))

;;all unique predicates in the X->ACE2 edges, only 29 unique ones
(define X->ACE2/preds
(remove-duplicates (map (lambda (ls) (list-ref ls 2)) X->ACE2/simple)))

;; all Gene concept X's + synonyms in the X--pred-->ACE2 edges
;; seems like there are 57 gene concepts
(define 1-hop-affector-genes/ACE2
  (remove-item
   '()
   (map (lambda (ls) (gene-filter ls '()))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->ACE2))) '()))

;; seems like there is a 1 to 1 ratio with HGNC ids, there are 57 HGNCs
(define 1-hop-affector-genes-HGNC/ACE2
  (remove-item
   '()
   (map (lambda (ls) (filter/curie ls '() "HGNC:"))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->ACE2))) '()))


;; 108 total: all Drug concept X's + synonyms in the X--pred-->ACE2 edges
;; question: what curie can we use to isolate drugs we want? DRUGBANK? CHEBI?
(define 1-hop-affector-drugs/ACE2
  (remove-item
   '()
   (map (lambda (ls) (drug-filter ls '()))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->ACE2))) '()))

(define 1-hop-affector-drugs-CHEBI/ACE2 
  (remove-item
   '()
   (map (lambda (ls) (filter/curie ls '() "CHEBI:"))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->ACE2))) '()))

;; 37 DRUGBANK curies  
(define 1-hop-affector-drugs-DRUGBANK/ACE2 
  (remove-item
   '()
   (map (lambda (ls) (filter/curie ls '() "DRUGBANK:"))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->ACE2))) '()))

|#

#|start 2-hop affector gene code here|#
#|
(define viral-entry-gene-ls
  '("HGNC:13557"))
|#




#|
;; 390 edges
(define TMPRSS2 (time (query/graph
                  ((X       #f)
                   (TMPRSS2 "HGNC:11876"))
                  ((X->TMPRSS2 #f))
                  (X X->TMPRSS2 TMPRSS2))))

(define 1-hop/concepts->TMPRSS2 (curies/query TMPRSS2 'X))

;; gives full edges
(define X->TMPRSS2 (edges/query TMPRSS2 'X->TMPRSS2))

(define X->TMPRSS2/simple
  (edge-matcher X->TMPRSS2 '()))

(define X->TMPRSS2/preds
  (remove-duplicates (map (lambda (ls) (list-ref ls 2)) X->TMPRSS2/simple)))

(define 1-hop-affector-genes/TMPRSS2
  (remove-item
   '()
   (map (lambda (ls) (gene-filter ls '()))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->TMPRSS2))) '()))

(define 1-hop-affector-drugs/TMPRSS2
  (remove-item
   '()
   (map (lambda (ls) (drug-filter ls '()))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->TMPRSS2))) '()))


;; 2236 edges 
(define CXCL10 (time (query/graph
                  ((X       #f)
                   (CXCL10 "HGNC:10637"))
                  ((X->CXCL10 #f))
                  (X X->CXCL10 CXCL10))))

(define 1-hop/concepts->CXCL10 (curies/query CXCL10 'X))

;; gives full edges
(define X->CXCL10 (edges/query CXCL10 'X->CXCL10))

(define X->CXCL10/simple
  (edge-matcher X->CXCL10 '()))

(define X->CXCL10/preds
  (remove-duplicates (map (lambda (ls) (list-ref ls 2)) X->CXCL10/simple)))

|#


#|
;;manually filtered list for X--decreases-->ACE2
'("targets"
  "inhibitor"
  "physically_interacts_with"
  "regulates_expression_of"
  "interacts_with"
  "directly_interacts_with"
  "decreases_activity_of"
  "affects"
  "associated_with"
  "inhibits"
  "coexists_with"
  "compared_with"
  "negatively_regulates")

;;manually filtered list for X--increases-->ACE2
'("targets"
  "physically_interacts_with"
  "regulates_expression_of"
  "interacts_with"
  "directly_interacts_with"
  "activator"
  "affects"
  "associated_with"
  "produces"
  "stimulates"
  "coexists_with"
  "positively_regulates"
  "positively_regulates__entity_to_entity")

|#






#|NOTES HERE|#

#|
Gives the all --predicate-->Object
(run* (p) (object-predicateo '(rtx
  1225271
  "NCIT:C102527"
  "ACE2 Gene"
  (11 . "http://w3id.org/biolink/vocab/GeneSet")
  (("iri" . "http://purl.obolibrary.org/obo/NCIT_C102527")
   ("synonym"
    .
    "['ACE2', 'Angiotensin I Converting Enzyme (Peptidyl-Dipeptidase A) 2 Gene']")
   ("category_label" . "gene_set")
   ("deprecated" . "False")
   ("description"
    .
    "This gene plays a role in both proteolysis and vasodilation.; UMLS Semantic Type: TUI:T028")
   ("provided_by" . "https://identifiers.org/umls/NCI")
   ("id" . "NCIT:C102527")
   ("update_date" . "2018")
   ("publications" . "[]")))  p))
'((rtx 15 . "subclass_of"))


(run* (p) (subject-predicateo
'(rtx
  1225271
  "NCIT:C102527"
  "ACE2 Gene"
  (11 . "http://w3id.org/biolink/vocab/GeneSet")
  (("iri" . "http://purl.obolibrary.org/obo/NCIT_C102527")
   ("synonym"
    .
    "['ACE2', 'Angiotensin I Converting Enzyme (Peptidyl-Dipeptidase A) 2 Gene']")
   ("category_label" . "gene_set")
   ("deprecated" . "False")
   ("description"
    .
    "This gene plays a role in both proteolysis and vasodilation.; UMLS Semantic Type: TUI:T028")
   ("provided_by" . "https://identifiers.org/umls/NCI")
   ("id" . "NCIT:C102527")
   ("update_date" . "2018")
   ("publications" . "[]")))  p))
'((rtx 3 . "xref")
 (rtx 15 . "subclass_of")
 (rtx 346 . "gene_encodes_gene_product")
 (rtx 354 . "gene_plays_role_in_process"))
|#


