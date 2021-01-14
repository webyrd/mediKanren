#lang racket
(require
  "../pieces-parts/query.rkt"
  "../pieces-parts/Thi-useful-functions.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*******************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*********GENERAL SEARCH********;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *** Data Sources:
;; - Processed CHAMP1 RNAseq data from outside of mediKanren
;; - RNA was obtained from patients or control
;;  cultured fibroblasts derived from skin biopsies
;;
;; *** Objective:
;; Repurpose drugs for CHAMP1 genetic disorder using RNAseq data from patients-derived skin fibroblasts
;;
;; *** Please find the case presentation at
;; First, I'm doing exploratory searches for the disease information related to CHAMP1 genes:
;; Find disease conditions that are related to CHAMP1 genes
(curie-synonyms/names "HGNC:20311")
(define CHAMP1 "HGNC:20311")
;; Check the diseases and conditions associted with the CHAMP1 gene
(define q-disease-CHAMP1 (time (query/graph
                     ((S disease)
                      (O CHAMP1))
                     ((S->O #f))
                     (S S->O O))))

(define disease-CHAMP1-edges (edges/query q-disease-CHAMP1 'S->O))

;; The results show that CHAMP1 gene (chromosome alignment-maintaining phosphoprotein, is a protein-coding gene,
;; a member of the zinc fingers C2H2-types protein, located on chromosome 13q34)
;; is associated with certain disease conditions such as
;; mental retardation (autosomal dominant), rare (non-syndromic) intellectual disability, monogenic genetic
;; central nervous system disorder, mental disorder, and psychiatric disorder. 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*******************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*******STRATEGY NUMBER 1*******;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In this strategy, we want to find drugs that target the genes that belong to enriched functional categories
;; such as cell cycle and cell senescence, retinol metabolism (which are results from gene set enrichemnet analysis (GSEA) performed
;; outside of medikanren).
;; GSEA has identified a number of genes that are upregulated in CHAMP1 fibroblasts compared to control fibroblasts.

;; First, find HGNC CURIEs ("HGNC:") of each gene from the gene name (either by using
;; genecards database "https://www.genecards.org/" or using mediKanren's find-concepts
;; ,then check if they are synonymized.
;; If the CURIE is synonymized, we use it to define the gene/protein in query/graph
;; eg:

(find-concepts #f (list "VIM"))
(curie-synonyms/names "HGNC:1583")


;; genes that are altered in the cell cycle pathway:
(define CCND2 "HGNC:1583") ;; cell cycle up-regulated
(define TUBA1A "HGNC:20766") ;; cell cycle down-regulated
;; cell senescence pathway:
;(define CCND2 "HGNC:1583") ;; up-regulated
(define RASSF5 "HGNC:17609") ;; up-regulated
;; DNA replication| apoptosis 
(define PSMA5 "HGNC:9534");; up-regulated
;; Oocyte meiosis | progesterone-mediated oocyte maturation| calcium signaling
(define ADCY4 "HGNC:235");; up-regulate
;; retinol metabolism:
(define ALDH1A1 "HGNC:402");; up-regulated
;; ABC transporter:
(define ABCA13 "HGNC:14638");; up-regulated
;; HIV infection| calcium
(define PTK2B "HGNC:9612");; up-regulated
(define AP1G2 "HGNC:556");; up-regulated
;; amyotropic lateral sclerosis | apoptosis
(define TNFRSF1B "HGNC:11917");; down-regulated
;; regulation of actin cytoskeleton:
(define FGF10 "HGNC:3666");;  up-regulated
(define CHRM2 "HGNC:1951");; down-regulated
;; Gonadotropin hormone-releasing
(define KCNN4 "HGNC:6293");; down-regulated
;; ECM receptor interaction:
(define CD36 "HGNC:1663");;  up-regulated
;; Neuroactive ligand-receptor interaction | calcium
(define EDNRB "HGNC:3180") ;;  up-regulated
(define LHB "HGNC:6584");; down-regulated
;; mRNA surveillance:
(define MSI2 "HGNC:18585") ;; down-regulated
;; calcium
;(define ADCY4 "HGNC:235");; up-regulated
;(define PTK2B "HGNC:9612");; up-regulated
(define ERBB4 "HGNC:3432");; up-regulated
;(define CHRM2 "HGNC:1951");; down-regulated
;(define EDNRB "HGNC:3180") ;;  up-regulated
;; apoptosis:
(define VIM "HGNC:12692");; down-regulated


;; All selected upregulated genes in enriched functional categories are:
(define functional-cat-up (list CCND2 RASSF5 PSMA5 ADCY4 ALDH1A1 ABCA13 PTK2B
                                AP1G2 FGF10 CD36 EDNRB EDNRB ERBB4))
(define functional-cat-down (list TUBA1A TNFRSF1B CHRM2 KCNN4 LHB MSI2 VIM))


(define results-CHAMP1-functional-cat-down (find-drugs-for-genes functional-cat-up positively-regulates))
(define results-CHAMP1-functional-cat-up (find-drugs-for-genes functional-cat-up negatively-regulates))
;;extract drug=>gene hash-table:
(define drug=>gene-CHAMP1-functional-cat-down (car results-CHAMP1-functional-cat-down))
(define drug=>gene-CHAMP1-functional-cat-up (car results-CHAMP1-functional-cat-up))
;;extract gene=>drug hash-table:
(define gene=>drug-CHAMP1-functional-cat-down (cadr results-CHAMP1-functional-cat-down))
(define gene=>drug-CHAMP1-functional-cat-up (car results-CHAMP1-functional-cat-up))
(define drug=>gene-strategy1 (merge-assoc-lists (list drug=>gene-CHAMP1-functional-cat-down drug=>gene-CHAMP1-functional-cat-up)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*******************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*******STRATEGY NUMBER 2*******;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Based on literature research, we identifed 5 different disease conditions that are associated with CHAMP1 genetic disorder:

(define hypotonia-concepts (find-concepts #f (list "hypotonia")))
(define cerebral-palsy-concepts (find-concepts #f (list "cerebral palsy")))
(define autism-concepts (find-concepts #f (list "autism")))
(define dev-delay-concepts (find-concepts #f (list "developmental delay")))
(define epilepsy-concepts (find-concepts #f (list "epilepsy")))


;; In this 2nd strategy, we first search for all the genes that are associated with the five CHAMP1
;; disease associated conditions using run/graph

(match-define
  (list name=>concepts name=>edges)
  (run/graph
   ((S #f)
    (O (set-union hypotonia-concepts
                  cerebral-palsy-concepts
                  autism-concepts
                  dev-delay-concepts
                  epilepsy-concepts)))
   ((S->O gene-to-disease-preds))
   (S S->O O)))

;; filter the subjects to retain only genes or proteins and get their names:
(map (lambda (y) (cons (concept->curie y) (concept->name y)))
     (filter (lambda (x)
               (gene/protein-concept? (concept->curie x))) (hash-ref name=>concepts 'S)))

;; This filter results in 1397 genes/proteins that are related to CHAMP1 patients' phenotypes.
;; I then cleaned the data, converted it to .csv and mapped different gene/protein identifers to HGNC symbols
;; then I removed duplicates and NA.
;; This resulted in 934 genes.
;; Using these genes, I plot a volcano plot that show differences in expression levels between
;; CHAMP1 patients fibroblasts vs control fibroblasts.
;; This visualisation shows genes that are statistically significantly altered between these
;; two conditions. These genes become our drug targets.


;; Now we want to find drugs that reverse the directions of DEG genes (which are genes found to be associated
;; with CHAMP1 patients' phenotypes- the results from the run/graph above.
;; Directionality is provided by gene expression values (determined by experiments on CHAMP1 patients' fibroblasts).
;; For genes that are significantly upregulated, we want to find drugs that decrease their expression, and vice versa.


;; Abstract genes that are upregulated:
(define CACNB4 "HGNC:1404")
(define PRICKLE1 "HGNC:17019")
(define DMD "HGNC:2928")
(define SHANK2 "HGNC:14295")
(define MAP2 "HGNC:6839")
(define H19 "HGNC:4713")
(define BCHE "HGNC:983")
(define EFNA5 "HGNC:3225")
(define CNTNAP2 "HGNC:13830")
(define ANKH "HGNC:15492")
(define SLC12A6 "HGNC:10914")
(define CHAMP1-phenotype-RNA-up
  (list CACNB4 PRICKLE1 DMD SHANK2 MAP2 H19 BCHE EFNA5 CNTNAP2 ANKH SLC12A6))

;; Alternatively, for genes that are significantly downregulatated, we want to find drugs that increase their expression: 
(define GABRA5 "HGNC:4079")
;(define TNFRSF1B "HGNC:11917")
(define STX1B "HGNC:18539")
(define RBFA "HGNC:26120")
(define CHAMP1-phenotype-RNA-down
  (list GABRA5 TNFRSF1B STX1B RBFA))



;; Now we use the function find-drugs-for-genes in Thi's-useful-functions:
(define results-CHAMP1-phenotype-RNA-up (find-drugs-for-genes CHAMP1-phenotype-RNA-up negatively-regulates))
(define drug=>gene-CHAMP1-phenotype-RNA-up (car results-CHAMP1-phenotype-RNA-up))
(define gene=>drug-CHAMP1-phenotype-RNA-up (cadr results-CHAMP1-phenotype-RNA-up))

(define results-CHAMP1-phenotype-RNA-down (find-drugs-for-genes CHAMP1-phenotype-RNA-down positively-regulates))
(define drug=>gene-CHAMP1-phenotype-RNA-down (car results-CHAMP1-phenotype-RNA-down))
(define gene=>drug-CHAMP1-phenotype-RNA-down (cadr results-CHAMP1-phenotype-RNA-down))

;; use the get-num-values function in Thi's-useful-functions:
(define drug=>gene-CHAMP1-phenotype-RNA-up-with-stats (get-num-values drug=>gene-CHAMP1-phenotype-RNA-up))
(define gene=>drug-CHAMP1-phenotype-RNA-up-with-stats (get-num-values gene=>drug-CHAMP1-phenotype-RNA-up))

(define drug=>gene-CHAMP1-phenotype-RNA-down-with-stats (get-num-values drug=>gene-CHAMP1-phenotype-RNA-down))
(define gene=>drug-CHAMP1-phenotype-RNA-down-with-stats (get-num-values gene=>drug-CHAMP1-phenotype-RNA-down))

(define merged_strategy2 (merge-2-assoc-list drug=>gene-CHAMP1-phenotype-RNA-up drug=>gene-CHAMP1-phenotype-RNA-down))
(define merged_trategy2_count (get-num-values (car merged_strategy2)))

(define merged_strategy2_genes (merge-2-assoc-list gene=>drug-CHAMP1-phenotype-RNA-up gene=>drug-CHAMP1-phenotype-RNA-down))
(define merged_trategy2_genes_count (get-num-values (car merged_strategy2_genes)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*******************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*******STRATEGY NUMBER 3*******;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2-edge queries to find drugs that treat the CHAMP1 disease conditions and reverse the gene expression directions:
;; These are all significantly altered genes in differential expression analyses performed outside of mediKanren

(define CHAMP1-DE-RNA-up (list "ENSEMBL:ENSG00000228630"
                               "ENSEMBL:ENSG00000261340"
                               "ENSEMBL:ENSG00000083937"
                               "ENSEMBL:ENSG00000143106"
                               "ENSEMBL:ENSG00000140199"
                               "ENSEMBL:ENSG00000065357"
                               "ENSEMBL:ENSG00000129480"
                               "ENSEMBL:ENSG00000162688"
                               "ENSEMBL:ENSG00000180818"
                               "ENSEMBL:ENSG00000136478"
                               "ENSEMBL:ENSG00000065413"
                               "ENSEMBL:ENSG00000167394"
                               "ENSEMBL:ENSG00000163249"
                               "ENSEMBL:ENSG00000118762"
                               "ENSEMBL:ENSG00000185070"
                               "ENSEMBL:ENSG00000089060"
                               "ENSEMBL:ENSG00000173221"
                               "ENSEMBL:ENSG00000175040"
                               "ENSEMBL:ENSG00000112964"
                               "ENSEMBL:ENSG00000111670"
                               "ENSEMBL:ENSG00000119986"
                               "ENSEMBL:ENSG00000106415"
                               "ENSEMBL:ENSG00000038427"
                               "ENSEMBL:ENSG00000154122"
                               "ENSEMBL:ENSG00000176531"
                               "ENSEMBL:ENSG00000121413"
                               "ENSEMBL:ENSG00000176809"
                               "ENSEMBL:ENSG00000165804"
                               "ENSEMBL:ENSG00000198948"
                               "ENSEMBL:ENSG00000012171"
                               "ENSEMBL:ENSG00000267454"
                               "ENSEMBL:ENSG00000182379"
                               "ENSEMBL:ENSG00000213983"
                               "ENSEMBL:ENSG00000169313"
                               "ENSEMBL:ENSG00000137573"
                               "ENSEMBL:ENSG00000174469"
                               "ENSEMBL:ENSG00000189129"
                               "ENSEMBL:ENSG00000112378"
                               "ENSEMBL:ENSG00000136048"
                               "ENSEMBL:ENSG00000101134"
                               "ENSEMBL:ENSG00000176928"
                               "ENSEMBL:ENSG00000184916"
                               "ENSEMBL:ENSG00000162643"
                               "ENSEMBL:ENSG00000266094"
                               "ENSEMBL:ENSG00000150540"
                               "ENSEMBL:ENSG00000176771"
                               "ENSEMBL:ENSG00000138772"
                               "ENSEMBL:ENSG00000115271"
                               "ENSEMBL:ENSG00000250133"
                               "ENSEMBL:ENSG00000179104"
                               "ENSEMBL:ENSG00000145390"
                               "ENSEMBL:ENSG00000184349"
                               "ENSEMBL:ENSG00000101605"
                               "ENSEMBL:ENSG00000205002"
                               "ENSEMBL:ENSG00000198133"
                               "ENSEMBL:ENSG00000205835"
                               "ENSEMBL:ENSG00000165113"
                               "ENSEMBL:ENSG00000172403"
                               "ENSEMBL:ENSG00000120899"
                               "ENSEMBL:ENSG00000174600"
                               "ENSEMBL:ENSG00000099260"
                               "ENSEMBL:ENSG00000137727"
                               "ENSEMBL:ENSG00000136160"
                               "ENSEMBL:ENSG00000182901"
                               "ENSEMBL:ENSG00000112981"
                               "ENSEMBL:ENSG00000242600"
                               "ENSEMBL:ENSG00000115604"
                               "ENSEMBL:ENSG00000144229"
                               "ENSEMBL:ENSG00000151623"
                               "ENSEMBL:ENSG00000165023"
                               "ENSEMBL:ENSG00000152804"
                               "ENSEMBL:ENSG00000112379"
                               "ENSEMBL:ENSG00000082497"
                               "ENSEMBL:ENSG00000144645"
                               "ENSEMBL:ENSG00000196208"
                               "ENSEMBL:ENSG00000228495"
                               "ENSEMBL:ENSG00000135919"
                               "ENSEMBL:ENSG00000211448"
                               "ENSEMBL:ENSG00000141449"
                               "ENSEMBL:ENSG00000203727"
                               "ENSEMBL:ENSG00000013588"
                               "ENSEMBL:ENSG00000130600"
                               "ENSEMBL:ENSG00000126861"
                               "ENSEMBL:ENSG00000165084"
                               "ENSEMBL:ENSG00000179869"
                               "ENSEMBL:ENSG00000114200"
                               "ENSEMBL:ENSG00000143028"
                               "ENSEMBL:ENSG00000164106"
                               "ENSEMBL:ENSG00000138449"
                               "ENSEMBL:ENSG00000110723"
                               "ENSEMBL:ENSG00000162687"
                               "ENSEMBL:ENSG00000183775"
                               "ENSEMBL:ENSG00000106538"
                               "ENSEMBL:ENSG00000064225"
                               "ENSEMBL:ENSG00000086289"
                               "ENSEMBL:ENSG00000070193"
                               "ENSEMBL:ENSG00000116183"
                               "ENSEMBL:ENSG00000102109"
                               "ENSEMBL:ENSG00000078018"
                               "ENSEMBL:ENSG00000099864"
                               "ENSEMBL:ENSG00000173391"
                               "ENSEMBL:ENSG00000198947"
                               "ENSEMBL:ENSG00000139174"
                               "ENSEMBL:ENSG00000165092"
                               "ENSEMBL:ENSG00000162105"
                               "ENSEMBL:ENSG00000137672"
                               "ENSEMBL:ENSG00000178568"
                               "ENSEMBL:ENSG00000064042"
                               "ENSEMBL:ENSG00000135218"
                               "ENSEMBL:ENSG00000126785"
                               "ENSEMBL:ENSG00000240694"
                               "ENSEMBL:ENSG00000248874"
                               "ENSEMBL:ENSG00000118971"
                               "ENSEMBL:ENSG00000182389"
                               "ENSEMBL:ENSG00000129467"
                               "ENSEMBL:ENSG00000106483"
                               "ENSEMBL:ENSG00000123572"))


(define CHAMP1-DE-RNA-down (list   "EMSEMBL:ENSG00000204792"
                                   "EMSEMBL:ENSG00000166796"
                                   "EMSEMBL:ENSG00000186297"
                                   "EMSEMBL:ENSG00000005379"
                                   "EMSEMBL:ENSG00000234465"
                                   "EMSEMBL:ENSG00000165887"
                                   "EMSEMBL:ENSG00000115648"
                                   "EMSEMBL:ENSG00000102575"
                                   "EMSEMBL:ENSG00000175084"
                                   "EMSEMBL:ENSG00000145824"
                                   "EMSEMBL:ENSG00000213030"
                                   "EMSEMBL:ENSG00000132639"
                                   "EMSEMBL:ENSG00000006606"
                                   "EMSEMBL:ENSG00000101017"
                                   "EMSEMBL:ENSG00000154451"
                                   "EMSEMBL:ENSG00000169429"
                                   "EMSEMBL:ENSG00000104826"
                                   "EMSEMBL:ENSG00000163618"
                                   "EMSEMBL:ENSG00000147573"
                                   "EMSEMBL:ENSG00000171517"
                                   "EMSEMBL:ENSG00000130720"
                                   "EMSEMBL:ENSG00000157557"
                                   "EMSEMBL:ENSG00000178882"
                                   "EMSEMBL:ENSG00000156453"
                                   "EMSEMBL:ENSG00000148803"
                                   "EMSEMBL:ENSG00000107611"
                                   "EMSEMBL:ENSG00000175294"
                                   "EMSEMBL:ENSG00000181072"
                                   "EMSEMBL:ENSG00000104783"
                                   "EMSEMBL:ENSG00000128285"
                                   "EMSEMBL:ENSG00000080823"
                                   "EMSEMBL:ENSG00000167889"
                                   "EMSEMBL:ENSG00000124191"
                                   "EMSEMBL:ENSG00000163701"
                                   "EMSEMBL:ENSG00000076706"
                                   "EMSEMBL:ENSG00000144354"
                                   "EMSEMBL:ENSG00000103034"
                                   "EMSEMBL:ENSG00000164929"
                                   "EMSEMBL:ENSG00000204103"
                                   "EMSEMBL:ENSG00000242265"
                                   "EMSEMBL:ENSG00000196460"
                                   "EMSEMBL:ENSG00000138395"
                                   "EMSEMBL:ENSG00000225968"
                                   "EMSEMBL:ENSG00000028137"
                                   "EMSEMBL:ENSG00000091622"
                                   "EMSEMBL:ENSG00000125968"
                                   "EMSEMBL:ENSG00000173848"
                                   "EMSEMBL:ENSG00000239332"
                                   "EMSEMBL:ENSG00000139278"
                                   "EMSEMBL:ENSG00000137563"
                                   "EMSEMBL:ENSG00000163814"
                                   "EMSEMBL:ENSG00000121904"
                                   "EMSEMBL:ENSG00000186847"
                                   "EMSEMBL:ENSG00000099365"
                                   "EMSEMBL:ENSG00000136490"
                                   "EMSEMBL:ENSG00000100181"
                                   "EMSEMBL:ENSG00000079462"
                                   "EMSEMBL:ENSG00000026025"
                                   "EMSEMBL:ENSG00000105255"
                                   "EMSEMBL:ENSG00000188064"
                                   "EMSEMBL:ENSG00000103449"
                                   "EMSEMBL:ENSG00000235162"
                                   "EMSEMBL:ENSG00000074410"
                                   "EMSEMBL:ENSG00000170961"
                                   "EMSEMBL:ENSG00000140280"
                                   "EMSEMBL:ENSG00000187583"
                                   "EMSEMBL:ENSG00000205683"
                                   "EMSEMBL:ENSG00000109062"
                                   "EMSEMBL:ENSG00000159231"
                                   "EMSEMBL:ENSG00000183077"
                                   "EMSEMBL:ENSG00000173588"
                                   "EMSEMBL:ENSG00000167552"
                                   "EMSEMBL:ENSG00000243156"
                                   "EMSEMBL:ENSG00000166886"
                                   "EMSEMBL:ENSG00000153944"
                                   "EMSEMBL:ENSG00000196912"
                                   "EMSEMBL:ENSG00000183506"
                                   "EMSEMBL:ENSG00000156535"
                                   "EMSEMBL:ENSG00000087266"
                                   "EMSEMBL:ENSG00000170921"
                                   "EMSEMBL:ENSG00000185052"
                                   "EMSEMBL:ENSG00000089486"
                                   "EMSEMBL:ENSG00000196155"
                                   "EMSEMBL:ENSG00000101546"
                                   "EMSEMBL:ENSG00000038382"
                                   "EMSEMBL:ENSG00000167460"
                                   "EMSEMBL:ENSG00000186281"
                                   "EMSEMBL:ENSG00000130309"
                                   "EMSEMBL:ENSG00000054523"
                                   "EMSEMBL:ENSG00000100097"
                                   "EMSEMBL:ENSG00000142892"
                                   "EMSEMBL:ENSG00000231419"
                                   "EMSEMBL:ENSG00000163687"))

;; define CHAMP1 disease related conditions:
(define dev-delay "HP:0001263")
(define hypotonia "HP:0001252")
(define cerebral-palsy "HP:0100021")
(define autism "MONDO:0005260")
(define epilepsy "MONDO:0005027")

;; The function find-drug-for-condition takes a disease condition, a gene list (derived from RNA-seq DEG results)
;; a list of drug-to-gene predicates that we want the drug to modulate.
;; This function will run a query/graph that does a 2-edge query which find drugs that modify the disease condition
;; while also modulate the gene in the gene list
;; this function will return a list which contains 2 hash-tables which are sorted based on the number
;; of genes or drugs available:

; Use the find-drug-for-condition from Thi-useful-functions.rkt:
(define dev-delay-drug-results-RNA-up (find-drug-for-condition dev-delay CHAMP1-DE-RNA-up negatively-regulates))
(define dev-delay-drug-results-RNA-down (find-drug-for-condition dev-delay CHAMP1-DE-RNA-down positively-regulates))

(define hypotonia-drug-results-RNA-up (find-drug-for-condition hypotonia CHAMP1-DE-RNA-up negatively-regulates))
(define hypotonia-drug-results-RNA-down (find-drug-for-condition hypotonia CHAMP1-DE-RNA-up negatively-regulates))

(define cerebral-palsy-drug-results-RNA-up (find-drug-for-condition cerebral-palsy CHAMP1-DE-RNA-up negatively-regulates))
(define cerebral-palsy-drug-results-RNA-down (find-drug-for-condition cerebral-palsy CHAMP1-DE-RNA-down positively-regulates))

(define autism-drug-results-RNA-up (find-drug-for-condition autism CHAMP1-DE-RNA-up negatively-regulates))
(define autism-drug-results-RNA-down (find-drug-for-condition autism CHAMP1-DE-RNA-down positively-regulates))

(define epilepsy-drug-results-RNA-up (find-drug-for-condition epilepsy CHAMP1-DE-RNA-up negatively-regulates))
(define epilepsy-drug-results-RNA-down (find-drug-for-condition epilepsy CHAMP1-DE-RNA-down positively-regulates))

;; Extract gene=>drug and drug=>gene tables:

(define drug=>gene-dev-delay-drug-results-RNA-up (car dev-delay-drug-results-RNA-up))
(define gene=>drug-dev-delay-drug-results-RNA-up (cadr dev-delay-drug-results-RNA-up))
(define drug=>gene-dev-delay-drug-results-RNA-down (car dev-delay-drug-results-RNA-down))
(define gene=>drug-dev-delay-drug-results-RNA-down (cadr dev-delay-drug-results-RNA-down))

(define drug=>gene-hypotonia-drug-results-RNA-up (car hypotonia-drug-results-RNA-up))
(define gene=>drug-hypotonia-drug-results-RNA-up (cadr hypotonia-drug-results-RNA-up))
(define drug=>gene-hypotonia-drug-results-RNA-down (car hypotonia-drug-results-RNA-down))
(define gene=>drug-hypotonia-drug-results-RNA-down (cadr hypotonia-drug-results-RNA-down))

(define drug=>gene-cerebral-palsy-drug-results-RNA-up (car cerebral-palsy-drug-results-RNA-up))
(define gene=>drug-cerebral-palsy-drug-results-RNA-up (cadr cerebral-palsy-drug-results-RNA-up))
(define drug=>gene-cerebral-palsy-drug-results-RNA-down (car cerebral-palsy-drug-results-RNA-down))
(define gene=>drug-cerebral-palsy-drug-results-RNA-down (cadr cerebral-palsy-drug-results-RNA-down))

(define drug=>gene-autism-drug-results-RNA-up (car autism-drug-results-RNA-up))
(define gene=>drug-autism-drug-results-RNA-up (cadr autism-drug-results-RNA-up))
(define drug=>gene-autism-drug-results-RNA-down (car autism-drug-results-RNA-down))
(define gene=>drug-autism-drug-results-RNA-down (cadr autism-drug-results-RNA-down))

(define drug=>gene-epilepsy-drug-results-RNA-up (car epilepsy-drug-results-RNA-up))
(define gene=>drug-epilepsy-drug-results-RNA-up (cadr epilepsy-drug-results-RNA-up))
(define drug=>gene-epilepsy-drug-results-RNA-down (car epilepsy-drug-results-RNA-down))
(define gene=>drug-epilepsy-drug-results-RNA-down (cadr epilepsy-drug-results-RNA-down))

; combine 10 association lists:
(define strategy3_results
  (merge-assoc-lists
   (list drug=>gene-dev-delay-drug-results-RNA-up
         drug=>gene-dev-delay-drug-results-RNA-down
         drug=>gene-hypotonia-drug-results-RNA-up 
         drug=>gene-hypotonia-drug-results-RNA-down
         drug=>gene-cerebral-palsy-drug-results-RNA-up
         drug=>gene-cerebral-palsy-drug-results-RNA-down
         drug=>gene-autism-drug-results-RNA-up
         drug=>gene-autism-drug-results-RNA-down
         drug=>gene-epilepsy-drug-results-RNA-up
         drug=>gene-epilepsy-drug-results-RNA-down)))


(define strategy3_genes_results
  (merge-assoc-lists
   (list gene=>drug-dev-delay-drug-results-RNA-up
         gene=>drug-dev-delay-drug-results-RNA-down
         gene=>drug-hypotonia-drug-results-RNA-up 
         gene=>drug-hypotonia-drug-results-RNA-down
         gene=>drug-cerebral-palsy-drug-results-RNA-up
         gene=>drug-cerebral-palsy-drug-results-RNA-down
         gene=>drug-autism-drug-results-RNA-up
         gene=>drug-autism-drug-results-RNA-down
         gene=>drug-epilepsy-drug-results-RNA-up
         gene=>drug-epilepsy-drug-results-RNA-down)))

; sort them by the values:
(define strategy3_results-count (get-num-values (car strategy3_results)))
(define strategy3_results-genes-count (get-num-values (car strategy3_genes_results)))

