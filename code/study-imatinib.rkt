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

(define rem-dups
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(member (car ls) (cdr ls)) (rem-dups (cdr ls))]
      [else (cons (car ls) (rem-dups (cdr ls)))])))

(define union
  (lambda (l1 l2)
    (cond
      [(null? l1) l2]
      [(member (car l1) l2) (union (cdr l1) l2)]
      [else (cons (car l1) (union (cdr l1) l2))])))

(define union*
  (lambda args
    (union*-aux args)))

(define union*-aux
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(null? (cdr ls)) (car ls)]
      [else (union (car ls) (union*-aux (cdr ls)))])))

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









;; Greg says:

;; the structure of an edge is:
;; (subject object predicate subject-type object-type pred-info)

;; So instead of looking for "gngm" in the type list of a concept, you can instead use subject-type or object-type to constrain the edge itself.
;; so:
;; (== `(,s-imatinib/gene ,m-imatinib/gene ,p-imatinib/gene . ,e-rest-imatinib/gene) e-imatinib/gene)

;; becomes (with fresh st-imatinib/gene):
;; (== `(,s-imatinib/gene ,m-imatinib/gene ,p-imatinib/gene st-imatinib/gene "gngm". ,e-rest-imatinib/gene) e-imatinib/gene)

;; then you no longer have to use membero



;; Will says:

;; What Greg says above, but stronger!  Using member can allow too many entry types--think of it as a "fuzzy" version of a query!  You asked for a gene?  Well, here are genes, and proteins, and ...



;;; one step at a time!

;; TODO all direct edges from all types of imatinib

;; TODO count the number of times each gene appears inhibited, across all versions of imatinib

;; TODO find all the subgraphs of the form 'some variant of imatinib INHIBITS some specific gene which CAUSES some specific disease or neoplasm which is directly TREATED by that version of imatinib'.  Ideally, fully explore most specific entities before broadening to categories.


;; Ah!  Now Will is enlightened!  I *need* to use "gngm" as the object type in the query, rather than calling membero on the list of associated types.
;; This will keep me from accidentally picking up aapp|T116|Amino Acid, Peptide, or Protein, for example, when I want genes.





;; TODO try strategy of most-specific to least-specific, based on branching factor for the next "hop" during a query




;; TODO intersection of relatively specific "dsyn", "neop", and "patf" directly treated by the relatively specific imatinib synonyms with the relatively specific "dsyn", "neop", and "patf" directly caused by the relatively specific genes (or gene groups) inhibited by the relatively specific imatinib synonyms



;; TODO relatively specific "dsyn", "neop", and "patf" that are directly caused by relatively specific genes inhibited by relatively specific imatinib synonyms


;; TODO intersection of "dsyn", "neop", and "patf" directly treated by the imatinib synonyms with the "dsyn", "neop", and "patf" directly caused by the genes (or gene groups) inhibited by the imatinib synonyms


;; TODO "dsyn", "neop", and "patf" that are directly caused by the genes (or gene groups) inhibited by the imatinib synonyms



;; TODO: How many of the cell functions below for KIT/C-KIT are unique?  And how many of those are specific enough to be meaningful/useful?
;; try taking the union* of all the non-silly entries

;; There are 47 genes are directly inhibited by Gleevec *and* directly cause diseases that Gleevec directly treats
;; (vs. 50 genes that are inhibited directly by Gleevec).  (See query below that produces these answers.)
;;
;; For each gene, how many cell functions (celf|T043|Cell Function) are directly caused by that gene?
(sort
 (map
  (lambda (gene)
    (let ((cell-functions
           (run* (q)
             (fresh (cell-function e-gene/cell-function st-gene/cell-function ot-gene/cell-function e-gene/cell-function-rest)
               (== cell-function q)
               (== "celf" ot-gene/cell-function)              
               (== `(,gene ,cell-function "CAUSES" ,st-gene/cell-function ,ot-gene/cell-function . ,e-gene/cell-function-rest) e-gene/cell-function)
               (edgeo e-gene/cell-function)))))
      (list (length (rem-dups cell-functions)) gene)))
  '((1428985 "PDGFD gene" ("aapp" "gngm"))
    (919477 "LCK gene" ("aapp" "enzy" "gngm"))
    (1136340 "Semaphorins" ("bacs" "gngm" "aapp"))
    (1366876 "MAPK14 gene" ("gngm" "aapp" "enzy"))
    (1364818 "APP gene" ("enzy" "gngm" "bacs" "aapp" "imft"))
    (1333568 "FLT3 gene" ("gngm" "phsu" "bacs" "aapp"))
    (79050 "c-abl Proto-Oncogenes" ("aapp" "gngm"))
    (79413 "Genes, abl" ("gngm" "aapp"))
    (812253 "CRKL gene" ("bacs" "aapp" "gngm"))
    (915156 "Ephrin Receptor EphA8" ("gngm" "enzy" "aapp"))
    (2716 "Amyloid" ("bacs" "aapp" "gngm"))
    (3241 "Antibodies" ("gngm" "aapp" "imft"))
    (33640 "PROTEIN KINASE" ("gngm" "enzy" "aapp"))
    (33681 "Protein Tyrosine Kinase" ("enzy" "gngm" "aapp"))
    (164786 "Proto-Oncogene Proteins c-akt" ("gngm" "aapp" "enzy"))
    (33684 "Proteins" ("bacs" "gngm" "aapp"))
    (246681 "platelet-derived growth factor BB" ("gngm" "phsu" "aapp"))
    (290068
     "Platelet-Derived Growth Factor beta Receptor"
     ("aapp" "gngm" "rcpt" "enzy"))
    (812228 "AKT1 gene" ("aapp" "phsu" "enzy" "gngm" "bacs"))
    (812375 "ELK3 gene" ("enzy" "gngm" "bacs" "aapp"))
    (1335239 "PPBP gene" ("bacs" "aapp" "gngm"))
    (1419240 "RAD51 gene" ("enzy" "gngm" "aapp"))
    (1421416 "UVRAG gene" ("gngm" "phsu" "aapp"))
    (1422009 "TP63 gene" ("rcpt" "phsu" "imft" "aapp" "gngm"))
    (1424677 "CKAP4 gene" ("gngm" "aapp" "bacs" "phsu"))
    (1425835 "KCNH8 gene" ("gngm" "aapp" "bacs"))
    (1439347 "BTG1 gene" ("gngm" "aapp"))
    (4891 "Fusion Proteins, bcr-abl" ("aapp" "gngm" "bacs"))
    (1439337 "tyrosine kinase ABL1" ("aapp" "gngm" "enzy"))
    (80092
     "Macrophage Colony-Stimulating Factor Receptor"
     ("enzy" "aapp" "imft" "gngm"))
    (879468 "CSF1R gene" ("aapp" "imft" "rcpt" "gngm" "enzy"))
    (32200 "Platelet-Derived Growth Factor" ("gngm" "aapp" "bacs"))
    (72470 "Proto-Oncogene Protein c-kit" ("aapp" "gngm" "rcpt" "imft"))
    (206364 "Receptor Protein-Tyrosine Kinases" ("enzy" "rcpt" "gngm" "aapp"))
    (290067
     "Platelet-Derived Growth Factor alpha Receptor"
     ("rcpt" "aapp" "gngm" "enzy"))
    (174680 "Cyclin D1" ("gngm" "bacs" "aapp"))
    (812385 "BCR gene" ("gngm" "bacs" "enzy" "aapp"))
    (1335202 "PDGFRB gene" ("bacs" "gngm" "rcpt" "enzy" "aapp"))
    (597357 "receptor" ("aapp" "gngm" "rcpt"))
    (31727 "Phosphotransferases" ("aapp" "gngm" "enzy"))
    (1412097 "ABL1 gene" ("imft" "enzy" "gngm" "aapp" "bacs" "phsu"))
    (71253 "Platelet-Derived Growth Factor Receptor" ("aapp" "gngm" "enzy"))
    (1826328 "MTTP gene" ("aapp" "lipd" "gngm" "imft" "phsu" "bacs"))
    (79427 "Tumor Suppressor Genes" ("gngm" "aapp"))
    (105770 "beta catenin" ("aapp" "gngm" "bacs"))
    (920288 "C-KIT Gene" ("gngm" "aapp"))
    (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))))
 (lambda (l1 l2) (< (car l1) (car l2))))
=>
'((1 (79413 "Genes, abl" ("gngm" "aapp")))
  (2
   (290067
    "Platelet-Derived Growth Factor alpha Receptor"
    ("rcpt" "aapp" "gngm" "enzy")))
  (3 (1425835 "KCNH8 gene" ("gngm" "aapp" "bacs")))
  (3 (1335202 "PDGFRB gene" ("bacs" "gngm" "rcpt" "enzy" "aapp")))
  (4 (1428985 "PDGFD gene" ("aapp" "gngm")))
  (4 (1136340 "Semaphorins" ("bacs" "gngm" "aapp")))
  (4 (812375 "ELK3 gene" ("enzy" "gngm" "bacs" "aapp")))
  (4 (1439347 "BTG1 gene" ("gngm" "aapp")))
  (5 (79050 "c-abl Proto-Oncogenes" ("aapp" "gngm")))
  (7 (4891 "Fusion Proteins, bcr-abl" ("aapp" "gngm" "bacs")))
  (7 (71253 "Platelet-Derived Growth Factor Receptor" ("aapp" "gngm" "enzy")))
  (7 (920288 "C-KIT Gene" ("gngm" "aapp")))
  (8 (1333568 "FLT3 gene" ("gngm" "phsu" "bacs" "aapp")))
  (8 (1335239 "PPBP gene" ("bacs" "aapp" "gngm")))
  (8 (1419240 "RAD51 gene" ("enzy" "gngm" "aapp")))
  (8 (1439337 "tyrosine kinase ABL1" ("aapp" "gngm" "enzy")))
  (8
   (80092
    "Macrophage Colony-Stimulating Factor Receptor"
    ("enzy" "aapp" "imft" "gngm")))
  (9 (915156 "Ephrin Receptor EphA8" ("gngm" "enzy" "aapp")))
  (10 (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp")))
  (11 (812253 "CRKL gene" ("bacs" "aapp" "gngm")))
  (11
   (290068
    "Platelet-Derived Growth Factor beta Receptor"
    ("aapp" "gngm" "rcpt" "enzy")))
  (11 (1424677 "CKAP4 gene" ("gngm" "aapp" "bacs" "phsu")))
  (11 (72470 "Proto-Oncogene Protein c-kit" ("aapp" "gngm" "rcpt" "imft")))
  (12 (1421416 "UVRAG gene" ("gngm" "phsu" "aapp")))
  (12 (879468 "CSF1R gene" ("aapp" "imft" "rcpt" "gngm" "enzy")))
  (14 (1826328 "MTTP gene" ("aapp" "lipd" "gngm" "imft" "phsu" "bacs")))
  (15 (919477 "LCK gene" ("aapp" "enzy" "gngm")))
  (17 (1422009 "TP63 gene" ("rcpt" "phsu" "imft" "aapp" "gngm")))
  (18 (33681 "Protein Tyrosine Kinase" ("enzy" "gngm" "aapp")))
  (19 (1412097 "ABL1 gene" ("imft" "enzy" "gngm" "aapp" "bacs" "phsu")))
  (24 (246681 "platelet-derived growth factor BB" ("gngm" "phsu" "aapp")))
  (26 (174680 "Cyclin D1" ("gngm" "bacs" "aapp")))
  (28 (2716 "Amyloid" ("bacs" "aapp" "gngm")))
  (30
   (206364 "Receptor Protein-Tyrosine Kinases" ("enzy" "rcpt" "gngm" "aapp")))
  (33 (812385 "BCR gene" ("gngm" "bacs" "enzy" "aapp")))
  (33 (79427 "Tumor Suppressor Genes" ("gngm" "aapp")))
  (34 (1366876 "MAPK14 gene" ("gngm" "aapp" "enzy")))
  (39 (1364818 "APP gene" ("enzy" "gngm" "bacs" "aapp" "imft")))
  (43 (32200 "Platelet-Derived Growth Factor" ("gngm" "aapp" "bacs")))
  (45 (105770 "beta catenin" ("aapp" "gngm" "bacs")))
  (52 (33640 "PROTEIN KINASE" ("gngm" "enzy" "aapp")))
  (57 (164786 "Proto-Oncogene Proteins c-akt" ("gngm" "aapp" "enzy")))
  (71 (812228 "AKT1 gene" ("aapp" "phsu" "enzy" "gngm" "bacs")))
  ;; marginal at best
  (80 (31727 "Phosphotransferases" ("aapp" "gngm" "enzy")))
  ;; below here are silly answers
  (100 (597357 "receptor" ("aapp" "gngm" "rcpt")))
  (104 (3241 "Antibodies" ("gngm" "aapp" "imft")))
  (174 (33684 "Proteins" ("bacs" "gngm" "aapp"))))



;; only 14 dsyn, neop, or patf directly caused by a celf,
;; and many of these are so general as to be almost meaningless
(rem-dups (run* (q)
  (fresh (e subj obj pred st ot rest)
    (== (list subj obj) q)
    (== "celf" st)
    (conde
      [(== "dsyn" ot)]
      [(== "neop" ot)]
      [(== "patf" ot)])
    (== "CAUSES" pred)
    (== `(,subj ,obj ,pred ,st ,ot . ,rest) e)
    (edgeo e))))
'(((3272 "Antibody -dependent cell cytotoxicity" ("celf"))
   (11854 "Diabetes Mellitus, Insulin-Dependent" ("dsyn")))
  ((7613 "Cell physiology" ("celf"))
   (4153 "Atherosclerosis" ("dsyn")))
  ((7577 "Cell Adhesion" ("celf"))
   (242184 "Hypoxia" ("patf")))
  ((7577 "Cell Adhesion" ("celf"))
   (852964 "Shunt occlusion" ("patf")))
  ((24262 "Lymphocyte Activation" ("celf"))
   (20517 "Hypersensitivity" ("patf")))
  ((920567 "leukocyte activation" ("celf"))
   (332448 "Infiltration" ("patf")))
  ((1159339 "Protein Secretion" ("celf"))
   (277785 "Functional disorder" ("patf")))
  ((7613 "Cell physiology" ("celf"))
   (12634 "Disease" ("dsyn")))
  ((7613 "Cell physiology" ("celf"))
   (42769 "Virus Diseases" ("dsyn")))
  ((7613 "Cell physiology" ("celf"))
   (524851 "Neurodegenerative Disorders" ("dsyn")))
  ((7613 "Cell physiology" ("celf"))
   (751651 "Mitochondrial Diseases" ("dsyn")))
  ((1154987 "anti-inflammatory response" ("celf"))
   (21290 "Infant, Newborn, Diseases" ("dsyn")))
  ((1158770 "Transcriptional Regulation" ("celf"))
   (42373 "Vascular Diseases" ("dsyn")))
  ((1524026 "Metabolic Process, Cellular" ("celf"))
   (12634 "Disease" ("dsyn"))))

;; 9 results
(run* (q)
  (fresh (e subj obj pred st ot rest)
    (== e q)
    (fuzzy-concepto "mast cell activation" obj)
    (conde
      [(== "dsyn" st)]
      [(== "neop" st)]
      [(== "patf" st)])
    (== "MANIFESTATION_OF" pred)
    (== `(,subj ,obj ,pred ,st ,ot . ,rest) e)
    (edgeo e)))
=>
'(((4096 "Asthma" ("dsyn"))
   (1155074 "mast cell activation" ("celf"))
   "MANIFESTATION_OF"
   "dsyn"
   "celf"
   (2741114))
  ((12634 "Disease" ("dsyn"))
   (1155074 "mast cell activation" ("celf"))
   "MANIFESTATION_OF"
   "dsyn"
   "celf"
   (12217411))
  ((36221 "Mast-Cell Sarcoma" ("neop"))
   (1155074 "mast cell activation" ("celf"))
   "MANIFESTATION_OF"
   "neop"
   "celf"
   (26659448))
  ((343378 "Helicobacter-associated gastritis" ("dsyn"))
   (1155074 "mast cell activation" ("celf"))
   "MANIFESTATION_OF"
   "dsyn"
   "celf"
   (21932987))
  ((13604 "Edema" ("patf"))
   (1155074 "mast cell activation" ("celf"))
   "MANIFESTATION_OF"
   "patf"
   "celf"
   (21130119))
  ((745283 "INFECTIOUS PROCESS" ("dsyn"))
   (1155074 "mast cell activation" ("celf"))
   "MANIFESTATION_OF"
   "dsyn"
   "celf"
   (20706702))
  ((598934 "tumor growth" ("neop"))
   (1155074 "mast cell activation" ("celf"))
   "MANIFESTATION_OF"
   "neop"
   "celf"
   (24931643))
  ((242184 "Hypoxia" ("patf"))
   (1155074 "mast cell activation" ("celf"))
   "MANIFESTATION_OF"
   "patf"
   "celf"
   (16572929))
  ((332448 "Infiltration" ("patf"))
   (1155074 "mast cell activation" ("celf"))
   "MANIFESTATION_OF"
   "patf"
   "celf"
   (21130119)))


;; mast cell activation directly AFFECTS which disorders, neoplasms,
;; or pathologic functions?
;;
;; 35 results
(run* (q)
  (fresh (e subj obj pred st ot rest)
    (== e q)    
    (fuzzy-concepto "mast cell activation" subj)
    (== "AFFECTS" pred)
    (conde
      [(== "dsyn" ot)]
      [(== "neop" ot)]
      [(== "patf" ot)])
    (== `(,subj ,obj ,pred ,st ,ot . ,rest) e)
    (edgeo e)))


;; 41 results, none of which are CAUSES.  Most are AFFECTS.
;;
;; Results related to asthma include:
;;
;; ((1155074 "mast cell activation" ("celf"))
;;  (4096 "Asthma" ("dsyn"))
;;  "AFFECTS"
;;  "celf"
;;  "dsyn"
;;  (18209484 10352758))
;;
;; ((1155074 "mast cell activation" ("celf"))
;;  (4096 "Asthma" ("dsyn"))
;;  "ASSOCIATED_WITH"
;;  "celf"
;;  "dsyn"
;;  (2645347))
;;
;; ((1155074 "mast cell activation" ("celf"))
;;  (4099 "Asthma, Exercise-Induced" ("dsyn"))
;;  "NEG_AFFECTS"
;;  "celf"
;;  "dsyn"
;;  (1730841))
;;
;; ((1155074 "mast cell activation" ("celf"))
;;  (155877 "Extrinsic asthma NOS" ("dsyn"))
;;  "AFFECTS"
;;  "celf"
;;  "dsyn"
;;  (24509414))
;;
(run* (q)
  (fresh (e subj obj pred st ot rest)
    (== e q)
    (fuzzy-concepto "mast cell activation" subj)
    (conde
      [(== "dsyn" ot)]
      [(== "neop" ot)]
      [(== "patf" ot)])
    (== `(,subj ,obj ,pred ,st ,ot . ,rest) e)
    (edgeo e)))

;; find all direct edges between mast cell activation and asthma
;;
;; interesting!
;;
;; 'Asthma MANIFESTATION_OF mast cell activation' seems like the strongest claim,
;; followed by 'mast cell activation AFFECTS Asthma'
(run* (q)
  (fresh (e subj obj pred st ot rest)
    (== e q)
    (conde
      [(fuzzy-concepto "mast cell activation" subj)
       (fuzzy-concepto "asthma" obj)]
      [(fuzzy-concepto "asthma" subj)
       (fuzzy-concepto "mast cell activation" obj)])
    (== `(,subj ,obj ,pred ,st ,ot . ,rest) e)
    (edgeo e)))
=>
'(((1155074 "mast cell activation" ("celf"))
   (4096 "Asthma" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (18209484 10352758))
  ((1155074 "mast cell activation" ("celf"))
   (4096 "Asthma" ("dsyn"))
   "ASSOCIATED_WITH"
   "celf"
   "dsyn"
   (2645347))
  ((4096 "Asthma" ("dsyn"))
   (1155074 "mast cell activation" ("celf"))
   "AFFECTS"
   "dsyn"
   "celf"
   (17498066 17192558))
  ((4096 "Asthma" ("dsyn"))
   (1155074 "mast cell activation" ("celf"))
   "MANIFESTATION_OF"
   "dsyn"
   "celf"
   (2741114))
  ((1155074 "mast cell activation" ("celf"))
   (4099 "Asthma, Exercise-Induced" ("dsyn"))
   "NEG_AFFECTS"
   "celf"
   "dsyn"
   (1730841))
  ((1155074 "mast cell activation" ("celf"))
   (155877 "Extrinsic asthma NOS" ("dsyn"))
   "AFFECTS"
   "celf"
   "dsyn"
   (24509414)))



;; KIT or C-KIT directly causing some cell function (celf|T043|Cell Function)
;;
;; 22 results
(let ((diseases (run* (q)
                  (fresh (disease gene e-gene/disease st-gene/disease ot-gene/disease e-gene/disease-rest)
                    (== disease q)
                    (== "celf" ot-gene/disease)
                    (conde
                      [(== '(1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp")) gene)]
                      [(== '(920288 "C-KIT Gene" ("gngm" "aapp")) gene)]
                      [(== '(72470 "Proto-Oncogene Protein c-kit" ("aapp" "gngm" "rcpt" "imft")) gene)])
                    (== `(,gene ,disease "CAUSES" ,st-gene/disease ,ot-gene/disease . ,e-gene/disease-rest) e-gene/disease)
                    (edgeo e-gene/disease)))))
    (rem-dups diseases))
=>
'((7587 "Cell Death" ("celf"))
  (7620 "Cell Survival" ("celf"))
  (7595 "Cell Growth" ("celf"))
  (7608 "cell motility" ("celf"))
  (37083 "Signal Transduction" ("celf"))
  (13081 "Down-Regulation" ("celf"))
  (40682 "cell transformation" ("celf"))
  (7613 "Cell physiology" ("celf"))
  (86982 "Signal Transduction Pathways" ("moft" "celf"))
  (26255 "Mitosis" ("celf"))
  (1155074 "mast cell activation" ("celf"))
  (37080 "Signal Pathways" ("celf" "moft"))
  (1155781 "spindle assembly" ("celf"))
  (162638 "Apoptosis" ("celf"))
  (1155873 "Cell Cycle Arrest" ("celf"))
  (221117 "Anergy" ("celf"))
  (1514758 "Receptor Activation" ("celf"))
  (596290 "Cell Proliferation" ("celf"))
  (678903 "Neuronal Transmission" ("celf"))
  (949469 "Receptor Down-Regulation" ("moft" "celf"))
  (1155046 "T-Cell Proliferation" ("celf"))
  (1325893 "histamine secretion" ("celf")))

(let ((diseases (run* (q)
                  (fresh (disease gene e-gene/disease st-gene/disease ot-gene/disease e-gene/disease-rest)
                    (== disease q)
                    (conde
                      [(== '(1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp")) gene)]
                      [(== '(920288 "C-KIT Gene" ("gngm" "aapp")) gene)]
                      [(== '(72470 "Proto-Oncogene Protein c-kit" ("aapp" "gngm" "rcpt" "imft")) gene)])
                    (== `(,gene ,disease "CAUSES" ,st-gene/disease ,ot-gene/disease . ,e-gene/disease-rest) e-gene/disease)
                    (edgeo e-gene/disease)))))
    (length (rem-dups diseases)))
=>
104

;; which diseases, neoplasms, pathologic functions are directly caused by KIT or C-KIT?
;;
;; 62 results, including
;;
;; (12634 "Disease" ("dsyn"))
;; (678236 "Rare Diseases" ("dsyn"))
;; (879626 "Adverse effects" ("patf"))
;;
;; notice asthma isn't one of them!
(let ((diseases (run* (q)
                  (fresh (disease gene e-gene/disease st-gene/disease ot-gene/disease e-gene/disease-rest)
                    (== disease q)
                    (conde
                      [(== "dsyn" ot-gene/disease)]
                      [(== "neop" ot-gene/disease)]
                      [(== "patf" ot-gene/disease)])
                    (conde
                      [(== '(1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp")) gene)]
                      [(== '(920288 "C-KIT Gene" ("gngm" "aapp")) gene)]
                      [(== '(72470 "Proto-Oncogene Protein c-kit" ("aapp" "gngm" "rcpt" "imft")) gene)])
                    (== `(,gene ,disease "CAUSES" ,st-gene/disease ,ot-gene/disease . ,e-gene/disease-rest) e-gene/disease)
                    (edgeo e-gene/disease)))))
  (rem-dups diseases))
=>
'((2871 "Anemia" ("dsyn"))
  (11603 "Dermatitis" ("dsyn"))
  (1511 "Adhesions" ("acab" "dsyn"))
  (12634 "Disease" ("dsyn"))
  (16663 "Pathological fracture" ("dsyn"))
  (2874 "Aplastic Anemia" ("dsyn"))
  (17178 "Gastrointestinal Diseases" ("dsyn"))
  (4509 "Azoospermia" ("dsyn"))
  (37277 "Skin Diseases, Genetic" ("dsyn"))
  (41408 "Turner's Syndrome" ("cgab" "dsyn"))
  (11847 "Diabetes" ("dsyn"))
  (15625 "Fanconi's Anemia" ("dsyn"))
  (85215 "Ovarian Failure, Premature" ("dsyn"))
  (24899 "mastocytosis" ("dsyn"))
  (23435 "Leukemia, B-Cell, Acute" ("neop"))
  (25218 "Chloasma" ("dsyn"))
  (339789 "Congenital deafness" ("cgab" "dsyn"))
  (27051 "Myocardial Infarction" ("dsyn"))
  (29453 "Osteopenia" ("dsyn"))
  (43154 "Dental White Spot" ("dsyn"))
  (1136033 "Cutaneous Mastocytosis" ("dsyn"))
  (25286 "meningioma" ("neop"))
  (80024 "Piebaldism" ("dsyn"))
  (221013 "Mastocytosis, Systemic" ("dsyn"))
  (27022 "Myeloproliferative disease" ("neop"))
  (242354 "Congenital Disorders" ("dsyn"))
  (595978 "Idiopathic megacolon" ("dsyn"))
  (678236 "Rare Diseases" ("dsyn"))
  (948710 "Ureteropelvic junction obstruction" ("dsyn"))
  (7137 "Squamous cell carcinoma" ("neop"))
  (152254 "Fatty degeneration" ("patf"))
  (9402 "Carcinoma of the Large Intestine" ("neop"))
  (346421 "Chronic eosinophilic leukemia" ("neop"))
  (20507 "Hyperplasia" ("patf"))
  (1334699 "Mesenchymal Cell Neoplasm" ("neop"))
  (23467 "Leukemia, Myelocytic, Acute" ("neop"))
  (1140999 "Contraction" ("patf"))
  (20517 "Hypersensitivity" ("patf"))
  (27708 "Nephroblastoma" ("neop"))
  (27819 "Neuroblastoma" ("neop"))
  (6826 "Malignant Neoplasms" ("neop"))
  (25874 "Metrorrhagia" ("patf"))
  (37579 "Soft Tissue Neoplasms" ("neop"))
  (79218 "Fibromatosis, Aggressive" ("neop"))
  (151686 "Growth retardation" ("patf"))
  (85669 "Acute leukemia" ("neop"))
  (25202 "melanoma" ("neop"))
  (149925 "Small cell carcinoma of lung" ("neop"))
  (277785 "Functional disorder" ("patf"))
  (206728 "Plexiform Neurofibroma" ("neop"))
  (26986 "Dysmyelopoietic Syndromes" ("neop"))
  (679316 "MOTILITY DISORDER NOS" ("patf"))
  (334664 "Mast Cell Neoplasm" ("neop"))
  (27651 "Neoplasm" ("neop"))
  (345905 "Intrahepatic Cholangiocarcinoma" ("neop"))
  (699748 "Pathogenesis" ("patf"))
  (1261473 "sarcoma" ("neop"))
  (238198 "Gastrointestinal Stromal Tumors" ("neop"))
  (1326912 "Tumorigenesis" ("neop"))
  (879626 "Adverse effects" ("patf"))
  (1608408 "Malignant transformation" ("neop"))
  (1510411 "metaplastic cell transformation" ("patf")))


;; Combine two of the queries below: which of the Diseases or
;; Syndromes or Neoplastic Processes or Pathologic Functions directly
;; treated by imatinib synonyms are *directly* caused by the genes
;; directly inhibited by the imatinib synonyms?
(map
 (lambda (drug)
   (let ((diseases (run* (q)
                     (fresh (disease e-drug/disease p-st-drug/disease p-ob-drug/disease e-drug/disease-rest
                             gene e-drug/gene p-st-drug/gene e-drug/gene-rest
                             e-gene/disease st-gene/disease ot-gene/disease e-gene/disease-rest)
                       (== disease q)
                       (conde
                         [(== "dsyn" p-ob-drug/disease)]
                         [(== "neop" p-ob-drug/disease)]
                         [(== "patf" p-ob-drug/disease)])
                       (== `(,drug ,disease "TREATS" ,p-st-drug/disease ,p-ob-drug/disease . ,e-drug/disease-rest) e-drug/disease)
                       (== `(,drug ,gene "INHIBITS" ,p-st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
                       (== `(,gene ,disease "CAUSES" ,st-gene/disease ,ot-gene/disease . ,e-gene/disease-rest) e-gene/disease)
                       (edgeo e-drug/disease)
                       (edgeo e-drug/gene)
                       (edgeo e-gene/disease)))))
     (list (length (rem-dups diseases)) drug)))
 '((935989 "imatinib" ("phsu" "orch"))
   (939537 "Imatinib mesylate" ("orch" "phsu"))
   (385728 "CGP 57148" ("phsu" "orch"))
   (906802 "STI571" ("phsu" "orch"))
   (935987 "Gleevec" ("orch" "phsu"))))

(time (map
       (lambda (drug)
         (let ((diseases (run* (q)
                           (fresh (disease e-drug/disease p-st-drug/disease p-ob-drug/disease e-drug/disease-rest
                                           gene e-drug/gene p-st-drug/gene e-drug/gene-rest
                                           e-gene/disease st-gene/disease ot-gene/disease e-gene/disease-rest)
                             (== disease q)
                             (conde
                               [(== "dsyn" p-ob-drug/disease)]
                               [(== "neop" p-ob-drug/disease)]
                               [(== "patf" p-ob-drug/disease)])
                             (== `(,drug ,disease "TREATS" ,p-st-drug/disease ,p-ob-drug/disease . ,e-drug/disease-rest) e-drug/disease)
                             (== `(,drug ,gene "INHIBITS" ,p-st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
                             (== `(,gene ,disease "CAUSES" ,st-gene/disease ,ot-gene/disease . ,e-gene/disease-rest) e-gene/disease)
                             (edgeo e-drug/disease)
                             (edgeo e-drug/gene)
                             (edgeo e-gene/disease)))))
           (list (length (rem-dups diseases)) drug)))
       '(
         ;;(935989 "imatinib" ("phsu" "orch"))
         ;;(939537 "Imatinib mesylate" ("orch" "phsu"))
         ;;(385728 "CGP 57148" ("phsu" "orch"))
         ;;(906802 "STI571" ("phsu" "orch"))
         (935987 "Gleevec" ("orch" "phsu"))
         )))
cpu time: 7663 real time: 7680 gc time: 48
'((35 (935987 "Gleevec" ("orch" "phsu"))))

> (time (map
         (lambda (drug)
           (let ((diseases (run* (q)
                             (fresh (disease e-drug/disease p-st-drug/disease p-ob-drug/disease e-drug/disease-rest
                                             gene e-drug/gene p-st-drug/gene e-drug/gene-rest
                                             e-gene/disease st-gene/disease ot-gene/disease e-gene/disease-rest)
                               (== disease q)
                               (conde
                                 [(== "dsyn" p-ob-drug/disease)]
                                 [(== "neop" p-ob-drug/disease)]
                                 [(== "patf" p-ob-drug/disease)])
                               (== `(,drug ,disease "TREATS" ,p-st-drug/disease ,p-ob-drug/disease . ,e-drug/disease-rest) e-drug/disease)
                               (== `(,drug ,gene "INHIBITS" ,p-st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
                               (== `(,gene ,disease "CAUSES" ,st-gene/disease ,ot-gene/disease . ,e-gene/disease-rest) e-gene/disease)
                               (edgeo e-drug/disease)
                               (edgeo e-drug/gene)
                               (edgeo e-gene/disease)))))
             (rem-dups diseases)))
         '(
           ;;(935989 "imatinib" ("phsu" "orch"))
           ;;(939537 "Imatinib mesylate" ("orch" "phsu"))
           ;;(385728 "CGP 57148" ("phsu" "orch"))
           ;;(906802 "STI571" ("phsu" "orch"))
           (935987 "Gleevec" ("orch" "phsu"))
           )))
cpu time: 7748 real time: 7763 gc time: 56
'(((2395 "Alzheimer's Disease" ("dsyn"))
   (8679 "Chronic Disease" ("dsyn"))
   (5699 "Blast Phase" ("neop"))
   (11847 "Diabetes" ("dsyn"))
   (16059 "Fibrosis" ("patf"))
   (11860 "Diabetes Mellitus, Non-Insulin-Dependent" ("dsyn"))
   (37274 "skin disorder" ("dsyn"))
   (21655 "Insulin Resistance" ("patf"))
   (6826 "Malignant Neoplasms" ("neop"))
   (332448 "Infiltration" ("patf"))
   (920563 "insulin sensitivity" ("patf"))
   (7131 "Carcinoma, Non-Small-Cell Lung" ("neop"))
   (7134 "Renal Cell Carcinoma" ("neop"))
   (17636 "Glioblastoma" ("neop"))
   (23418 "leukemia" ("neop"))
   (23449 "Leukemia, Lymphocytic, Acute" ("neop"))
   (23467 "Leukemia, Myelocytic, Acute" ("neop"))
   (23473 "Myeloid Leukemia, Chronic" ("neop"))
   (23474 "Leukemia, Myeloid, Chronic-Phase" ("neop"))
   (25149 "medulloblastoma" ("neop"))
   (25202 "melanoma" ("neop"))
   (27627 "Neoplasm Metastasis" ("neop"))
   (27831 "Neurofibromatosis 1" ("neop"))
   (27651 "Neoplasm" ("neop"))
   (27859 "Acoustic Neuroma" ("neop"))
   (35335 "Retinoblastoma" ("neop"))
   (85669 "Acute leukemia" ("neop"))
   (153690 "Secondary malignant neoplasm of bone" ("neop"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   (279671 "Cervical Squamous Cell Carcinoma" ("neop"))
   (280100 "Solid tumor" ("neop"))
   (392784 "Dermatofibrosarcoma Protuberans" ("neop"))
   (677886 "Epithelial ovarian cancer" ("neop"))
   (856536 "Philadelphia chromosome positive" ("neop"))
   (1261473 "sarcoma" ("neop"))))


;; instead of diseases from the above query, lets look at the genes
;;
;; 47 genes are directly inhibited by Gleevec *and* directly cause diseases that Gleevec directly treats
;; (vs. 50 genes that are inhibited directly by Gleevec)
;;
;; still some slop:
;;
;; (3241 "Antibodies" ("gngm" "aapp" "imft"))
;; (33684 "Proteins" ("bacs" "gngm" "aapp"))
;; (597357 "receptor" ("aapp" "gngm" "rcpt"))
;;
;; At most 43 of these results are specific or semi-specific (and arguably fewer than 43).
;; 3 of these are KIT or C-KIT.
> (time (map
         (lambda (drug)
           (let ((genes (run* (q)
                             (fresh (disease e-drug/disease p-st-drug/disease p-ob-drug/disease e-drug/disease-rest
                                             gene e-drug/gene p-st-drug/gene e-drug/gene-rest
                                             e-gene/disease st-gene/disease ot-gene/disease e-gene/disease-rest)
                               (== gene q)
                               (conde
                                 [(== "dsyn" p-ob-drug/disease)]
                                 [(== "neop" p-ob-drug/disease)]
                                 [(== "patf" p-ob-drug/disease)])
                               (== `(,drug ,disease "TREATS" ,p-st-drug/disease ,p-ob-drug/disease . ,e-drug/disease-rest) e-drug/disease)
                               (== `(,drug ,gene "INHIBITS" ,p-st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
                               (== `(,gene ,disease "CAUSES" ,st-gene/disease ,ot-gene/disease . ,e-gene/disease-rest) e-gene/disease)
                               (edgeo e-drug/disease)
                               (edgeo e-drug/gene)
                               (edgeo e-gene/disease)))))
             (rem-dups genes)))
         '(
           ;;(935989 "imatinib" ("phsu" "orch"))
           ;;(939537 "Imatinib mesylate" ("orch" "phsu"))
           ;;(385728 "CGP 57148" ("phsu" "orch"))
           ;;(906802 "STI571" ("phsu" "orch"))
           (935987 "Gleevec" ("orch" "phsu"))
           )))
cpu time: 7566 real time: 7580 gc time: 43
'(((1428985 "PDGFD gene" ("aapp" "gngm"))
   (919477 "LCK gene" ("aapp" "enzy" "gngm"))
   (1136340 "Semaphorins" ("bacs" "gngm" "aapp"))
   (1366876 "MAPK14 gene" ("gngm" "aapp" "enzy"))
   (1364818 "APP gene" ("enzy" "gngm" "bacs" "aapp" "imft"))
   (1333568 "FLT3 gene" ("gngm" "phsu" "bacs" "aapp"))
   (79050 "c-abl Proto-Oncogenes" ("aapp" "gngm"))
   (79413 "Genes, abl" ("gngm" "aapp"))
   (812253 "CRKL gene" ("bacs" "aapp" "gngm"))
   (915156 "Ephrin Receptor EphA8" ("gngm" "enzy" "aapp"))
   (2716 "Amyloid" ("bacs" "aapp" "gngm"))
   (3241 "Antibodies" ("gngm" "aapp" "imft"))
   (33640 "PROTEIN KINASE" ("gngm" "enzy" "aapp"))
   (33681 "Protein Tyrosine Kinase" ("enzy" "gngm" "aapp"))
   (164786 "Proto-Oncogene Proteins c-akt" ("gngm" "aapp" "enzy"))
   (33684 "Proteins" ("bacs" "gngm" "aapp"))
   (246681 "platelet-derived growth factor BB" ("gngm" "phsu" "aapp"))
   (290068
    "Platelet-Derived Growth Factor beta Receptor"
    ("aapp" "gngm" "rcpt" "enzy"))
   (812228 "AKT1 gene" ("aapp" "phsu" "enzy" "gngm" "bacs"))
   (812375 "ELK3 gene" ("enzy" "gngm" "bacs" "aapp"))
   (1335239 "PPBP gene" ("bacs" "aapp" "gngm"))
   (1419240 "RAD51 gene" ("enzy" "gngm" "aapp"))
   (1421416 "UVRAG gene" ("gngm" "phsu" "aapp"))
   (1422009 "TP63 gene" ("rcpt" "phsu" "imft" "aapp" "gngm"))
   (1424677 "CKAP4 gene" ("gngm" "aapp" "bacs" "phsu"))
   (1425835 "KCNH8 gene" ("gngm" "aapp" "bacs"))
   (1439347 "BTG1 gene" ("gngm" "aapp"))
   (4891 "Fusion Proteins, bcr-abl" ("aapp" "gngm" "bacs"))
   (1439337 "tyrosine kinase ABL1" ("aapp" "gngm" "enzy"))
   (80092
    "Macrophage Colony-Stimulating Factor Receptor"
    ("enzy" "aapp" "imft" "gngm"))
   (879468 "CSF1R gene" ("aapp" "imft" "rcpt" "gngm" "enzy"))
   (32200 "Platelet-Derived Growth Factor" ("gngm" "aapp" "bacs"))
   (72470 "Proto-Oncogene Protein c-kit" ("aapp" "gngm" "rcpt" "imft"))
   (206364 "Receptor Protein-Tyrosine Kinases" ("enzy" "rcpt" "gngm" "aapp"))
   (290067
    "Platelet-Derived Growth Factor alpha Receptor"
    ("rcpt" "aapp" "gngm" "enzy"))
   (174680 "Cyclin D1" ("gngm" "bacs" "aapp"))
   (812385 "BCR gene" ("gngm" "bacs" "enzy" "aapp"))
   (1335202 "PDGFRB gene" ("bacs" "gngm" "rcpt" "enzy" "aapp"))
   (597357 "receptor" ("aapp" "gngm" "rcpt"))
   (31727 "Phosphotransferases" ("aapp" "gngm" "enzy"))
   (1412097 "ABL1 gene" ("imft" "enzy" "gngm" "aapp" "bacs" "phsu"))
   (71253 "Platelet-Derived Growth Factor Receptor" ("aapp" "gngm" "enzy"))
   (1826328 "MTTP gene" ("aapp" "lipd" "gngm" "imft" "phsu" "bacs"))
   (79427 "Tumor Suppressor Genes" ("gngm" "aapp"))
   (105770 "beta catenin" ("aapp" "gngm" "bacs"))
   (920288 "C-KIT Gene" ("gngm" "aapp"))
   (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))))

;; Disease or Syndromes/Neoplastic Processes or Pathologic Functions directly treated by imatinib synonyms.
;; (seems like "dsyn", "neop", and "patf" cover the actual disorders)
(map
 (lambda (drug)
   (let ((diseases (run* (q)
                     (fresh (disease e-drug/disease p-drug/disease p-st-drug/disease p-ob-drug/disease e-drug/disease-rest)
                       (== disease q)
                       (conde
                         [(== "dsyn" p-ob-drug/disease)]
                         [(== "neop" p-ob-drug/disease)]
                         [(== "patf" p-ob-drug/disease)])
                       (== `(,drug ,disease ,p-drug/disease ,p-st-drug/disease ,p-ob-drug/disease . ,e-drug/disease-rest) e-drug/disease)
                       (== "TREATS" p-drug/disease)
                       (edgeo e-drug/disease)))))
     (list (length (rem-dups diseases)) drug)))
 '((935989 "imatinib" ("phsu" "orch"))
   (939537 "Imatinib mesylate" ("orch" "phsu"))
   (385728 "CGP 57148" ("phsu" "orch"))
   (906802 "STI571" ("phsu" "orch"))
   (935987 "Gleevec" ("orch" "phsu"))))
=>
'((303 (935989 "imatinib" ("phsu" "orch")))
  (195 (939537 "Imatinib mesylate" ("orch" "phsu")))
  (5 (385728 "CGP 57148" ("phsu" "orch")))
  (65 (906802 "STI571" ("phsu" "orch")))
  (53 (935987 "Gleevec" ("orch" "phsu"))))


;; *anything* directly treated by imatinib synonyms (not restricted to Disease or Syndromes/Neoplastic Processes)
(map
 (lambda (drug)
   (let ((diseases (run* (q)
                     (fresh (disease e-drug/disease p-drug/disease p-st-drug/disease p-ob-drug/disease e-drug/disease-rest)
                       (== disease q)                                          
                       (== `(,drug ,disease ,p-drug/disease ,p-st-drug/disease ,p-ob-drug/disease . ,e-drug/disease-rest) e-drug/disease)
                       (== "TREATS" p-drug/disease)
                       (edgeo e-drug/disease)))))
     (list (length (rem-dups diseases)) drug)))
 '((935989 "imatinib" ("phsu" "orch"))
   (939537 "Imatinib mesylate" ("orch" "phsu"))
   (385728 "CGP 57148" ("phsu" "orch"))
   (906802 "STI571" ("phsu" "orch"))
   (935987 "Gleevec" ("orch" "phsu"))))
=>
'((430 (935989 "imatinib" ("phsu" "orch")))
  (247 (939537 "Imatinib mesylate" ("orch" "phsu")))
  (5 (385728 "CGP 57148" ("phsu" "orch")))
  (84 (906802 "STI571" ("phsu" "orch")))
  (65 (935987 "Gleevec" ("orch" "phsu"))))

;; Disease or Syndromes/Neoplastic Processes directly treated by imatinib synonyms.
(map
 (lambda (drug)
   (let ((diseases (run* (q)
                     (fresh (disease e-drug/disease p-drug/disease p-st-drug/disease p-ob-drug/disease e-drug/disease-rest)
                       (== disease q)                     
                       (conde
                         [(== "dsyn" p-ob-drug/disease)]
                         [(== "neop" p-ob-drug/disease)])
                       (== `(,drug ,disease ,p-drug/disease ,p-st-drug/disease ,p-ob-drug/disease . ,e-drug/disease-rest) e-drug/disease)
                       (== "TREATS" p-drug/disease)
                       (edgeo e-drug/disease)))))
     (list (length (rem-dups diseases)) drug)))
 '((935989 "imatinib" ("phsu" "orch"))
   (939537 "Imatinib mesylate" ("orch" "phsu"))
   (385728 "CGP 57148" ("phsu" "orch"))
   (906802 "STI571" ("phsu" "orch"))
   (935987 "Gleevec" ("orch" "phsu"))))
=>
'((277 (935989 "imatinib" ("phsu" "orch")))
  (184 (939537 "Imatinib mesylate" ("orch" "phsu")))
  (3 (385728 "CGP 57148" ("phsu" "orch")))
  (60 (906802 "STI571" ("phsu" "orch")))
  (49 (935987 "Gleevec" ("orch" "phsu"))))


;; if we remove "dsyn" and "neop", we see CGP 57148 also directly
;; treats patf-related disorders (patf|T046|Pathologic Function)
;;
;; CML and Respiratory Depression are the only two specific disorders, though.
(map
 (lambda (drug)
   (let ((diseases (run* (q)
                     (fresh (disease e-drug/disease p-drug/disease p-st-drug/disease p-ob-drug/disease e-drug/disease-rest)
                       (== e-drug/disease q)                     
                       (== `(,drug ,disease ,p-drug/disease ,p-st-drug/disease ,p-ob-drug/disease . ,e-drug/disease-rest) e-drug/disease)
                       (== "TREATS" p-drug/disease)
                       (edgeo e-drug/disease)))))
     diseases))
 '(
   ;;(935989 "imatinib" ("phsu" "orch"))
   ;;(939537 "Imatinib mesylate" ("orch" "phsu"))
   (385728 "CGP 57148" ("phsu" "orch"))
   ;;(906802 "STI571" ("phsu" "orch"))
   ;;(935987 "Gleevec" ("orch" "phsu"))
   ))
=>
'((((385728 "CGP 57148" ("phsu" "orch"))
    (12634 "Disease" ("dsyn"))
    "TREATS"
    "phsu"
    "dsyn"
    (8548747))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (23473 "Myeloid Leukemia, Chronic" ("neop"))
    "TREATS"
    "orch"
    "neop"
    (10501919))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (23474 "Leukemia, Myeloid, Chronic-Phase" ("neop"))
    "TREATS"
    "orch"
    "neop"
    (10501919))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (235063 "Respiratory Depression" ("patf"))
    "TREATS"
    "orch"
    "patf"
    (11477209))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (679222 "functional insufficiency" ("patf"))
    "TREATS"
    "orch"
    "patf"
    (12578692))))

;; there really is only one specific disease directly treated by CGP 57148
(map
 (lambda (drug)
   (let ((diseases (run* (q)
                     (fresh (disease e-drug/disease p-drug/disease p-st-drug/disease p-ob-drug/disease e-drug/disease-rest)

                       ;; (== disease q)
                       (== e-drug/disease q)
                     
                       (conde
                         [(== "dsyn" p-ob-drug/disease)]
                         [(== "neop" p-ob-drug/disease)])
                       (== `(,drug ,disease ,p-drug/disease ,p-st-drug/disease ,p-ob-drug/disease . ,e-drug/disease-rest) e-drug/disease)
                       (== "TREATS" p-drug/disease)
                       (edgeo e-drug/disease)))))
     diseases))
 '(
                                        ;(935989 "imatinib" ("phsu" "orch"))
                                        ;(939537 "Imatinib mesylate" ("orch" "phsu"))
   (385728 "CGP 57148" ("phsu" "orch"))
                                        ;(906802 "STI571" ("phsu" "orch"))
                                        ;(935987 "Gleevec" ("orch" "phsu"))
   ))
'((((385728 "CGP 57148" ("phsu" "orch"))
    (12634 "Disease" ("dsyn"))
    "TREATS"
    "phsu"
    "dsyn"
    (8548747))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (23473 "Myeloid Leukemia, Chronic" ("neop"))
    "TREATS"
    "orch"
    "neop"
    (10501919))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (23474 "Leukemia, Myeloid, Chronic-Phase" ("neop"))
    "TREATS"
    "orch"
    "neop"
    (10501919))))



;; So, insisting on "gngm" as the subject type cuts down on stray proteins when we want genes.  What about "orch" (orch|T109|Organic Chemical) vs. "phsu" (phsu|T121|Pharmacologic Substance) for the subject type?  For "CGP 57148", the INHIBITS edges always have "orch" as the subject type.  What about for "imatinib", "Imatinib mesylate", "STI571", and "Gleevec"?  There are a handful of uses of "phsu" as the edge type for the other aliases, but these seem more noise than a specific pattern.  Doesn't look like anything we can exploit.



;; the right way to do it
;; every drug other than "CGP 57148" INHIBITS both KIT and C-KIT
(map
  (lambda (drug)
    (let ((genes (run* (q)
                   (fresh (gene e-drug/gene p-drug/gene p-st-drug/gene e-drug/gene-rest)
                     (== gene q)
                     (== `(,drug ,gene ,p-drug/gene ,p-st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
                     (== "INHIBITS" p-drug/gene)
                     (edgeo e-drug/gene)))))
      (let ((genes (rem-dups genes)))
        (list (length genes)
              (and (member '(1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp")) genes) #t)
              (and (member '(920288 "C-KIT Gene" ("gngm" "aapp")) genes) #t)
              drug))))
  '((935989 "imatinib" ("phsu" "orch"))
    (939537 "Imatinib mesylate" ("orch" "phsu"))
    (385728 "CGP 57148" ("phsu" "orch"))
    (906802 "STI571" ("phsu" "orch"))
    (935987 "Gleevec" ("orch" "phsu"))))
=>
'((206 #t #t (935989 "imatinib" ("phsu" "orch")))
  (86 #t #t (939537 "Imatinib mesylate" ("orch" "phsu")))
  ;; Of the 5 results for CGP 57148, 3 of them are for specific genes.  0 of those 3 are for KIT or C-KIT.
  (5 #f #f (385728 "CGP 57148" ("phsu" "orch")))
  (61 #t #t (906802 "STI571" ("phsu" "orch")))
  ;; Of the 50 results for Gleevec, perhaps 38 of them are specific genes, rather than groupings of genes.
  ;; 3 of those 38 are for KIT or C-KIT.
  (50 #t #t (935987 "Gleevec" ("orch" "phsu"))))

;; the wrong way to do it
;; every drug other than "CGP 57148" INHIBITS both KIT and C-KIT
(map
  (lambda (drug)
    (let ((genes (run* (q)
                   (fresh (gene e-drug/gene p-drug/gene e-drug/gene-rest)                
                     (== gene q)                
                     (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                     (== "INHIBITS" p-drug/gene)
                     (edgeo e-drug/gene)
                     (fresh (cui name concept-type*)
                       (== `(,cui ,name ,concept-type*) gene)
                       (membero "gngm" concept-type*))))))
      (let ((genes (rem-dups genes)))
        (list (length genes)
              (and (member '(1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp")) genes) #t)
              (and (member '(920288 "C-KIT Gene" ("gngm" "aapp")) genes) #t)
              drug))))
  '((935989 "imatinib" ("phsu" "orch"))
    (939537 "Imatinib mesylate" ("orch" "phsu"))
    (385728 "CGP 57148" ("phsu" "orch"))
    (906802 "STI571" ("phsu" "orch"))
    (935987 "Gleevec" ("orch" "phsu"))))
=>
'((206 #t #t (935989 "imatinib" ("phsu" "orch")))
  (86 #t #t (939537 "Imatinib mesylate" ("orch" "phsu")))
  (8 #f #f (385728 "CGP 57148" ("phsu" "orch")))
  (61 #t #t (906802 "STI571" ("phsu" "orch")))
  (52 #t #t (935987 "Gleevec" ("orch" "phsu"))))

;; the right way to do it!
;; a few of the genes for some of the drugs are dups for "imatinib", "Imatinib mesylate", and "STI571"
(map
  (lambda (drug)
    (let ((genes (run* (q)
                   (fresh (gene e-drug/gene p-drug/gene p-st-drug/gene e-drug/gene-rest)                
                     (== gene q)
                     (== `(,drug ,gene ,p-drug/gene ,p-st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
                     (== "INHIBITS" p-drug/gene)
                     (edgeo e-drug/gene)))))
      (list (length (rem-dups genes)) drug)))
  '((935989 "imatinib" ("phsu" "orch"))
    (939537 "Imatinib mesylate" ("orch" "phsu"))
    (385728 "CGP 57148" ("phsu" "orch"))
    (906802 "STI571" ("phsu" "orch"))
    (935987 "Gleevec" ("orch" "phsu"))))
=>
'((206 (935989 "imatinib" ("phsu" "orch")))
  (86 (939537 "Imatinib mesylate" ("orch" "phsu")))
  (5 (385728 "CGP 57148" ("phsu" "orch")))
  (61 (906802 "STI571" ("phsu" "orch")))
  (50 (935987 "Gleevec" ("orch" "phsu"))))

;; the wrong way to do it!
;; a few of the genes for some of the drugs are dups
(map
  (lambda (drug)
    (let ((genes (run* (q)
                   (fresh (gene e-drug/gene p-drug/gene e-drug/gene-rest)                
                     (== gene q)                
                     (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                     (== "INHIBITS" p-drug/gene)
                     (edgeo e-drug/gene)
                     (fresh (cui name concept-type*)
                       (== `(,cui ,name ,concept-type*) gene)
                       (membero "gngm" concept-type*))))))
      (list (length (rem-dups genes)) drug)))
  '((935989 "imatinib" ("phsu" "orch"))
    (939537 "Imatinib mesylate" ("orch" "phsu"))
    (385728 "CGP 57148" ("phsu" "orch"))
    (906802 "STI571" ("phsu" "orch"))
    (935987 "Gleevec" ("orch" "phsu"))))
=>
'((206 (935989 "imatinib" ("phsu" "orch")))
  (86 (939537 "Imatinib mesylate" ("orch" "phsu")))
  (8 (385728 "CGP 57148" ("phsu" "orch")))
  (61 (906802 "STI571" ("phsu" "orch")))
  (52 (935987 "Gleevec" ("orch" "phsu"))))


;; The right way!  Notice there are fewer answers
(map
  (lambda (drug)
    (let ((genes (run* (q)
                   (fresh (gene e-drug/gene p-drug/gene p-st-drug/gene e-drug/gene-rest)
                     (== gene q)
                     (== `(,drug ,gene ,p-drug/gene ,p-st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
                     (== "INHIBITS" p-drug/gene)
                     (edgeo e-drug/gene)))))
      (list (length genes) drug)))
  '((935989 "imatinib" ("phsu" "orch"))
    (939537 "Imatinib mesylate" ("orch" "phsu"))
    (385728 "CGP 57148" ("phsu" "orch"))
    (906802 "STI571" ("phsu" "orch"))
    (935987 "Gleevec" ("orch" "phsu"))))
=>
'((213 (935989 "imatinib" ("phsu" "orch")))
  (88 (939537 "Imatinib mesylate" ("orch" "phsu")))
  (5 (385728 "CGP 57148" ("phsu" "orch")))
  (62 (906802 "STI571" ("phsu" "orch")))
  (50 (935987 "Gleevec" ("orch" "phsu"))))

;; The wrong way
(map
  (lambda (drug)
    (let ((genes (run* (q)
                   (fresh (gene e-drug/gene p-drug/gene e-drug/gene-rest)                
                     (== gene q)                
                     (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                     (== "INHIBITS" p-drug/gene)
                     (edgeo e-drug/gene)
                     (fresh (cui name concept-type*)
                       (== `(,cui ,name ,concept-type*) gene)
                       (membero "gngm" concept-type*))))))
      (list (length genes) drug)))
  '((935989 "imatinib" ("phsu" "orch"))
    (939537 "Imatinib mesylate" ("orch" "phsu"))
    (385728 "CGP 57148" ("phsu" "orch"))
    (906802 "STI571" ("phsu" "orch"))
    (935987 "Gleevec" ("orch" "phsu"))))
=>
'((219 (935989 "imatinib" ("phsu" "orch")))
  (90 (939537 "Imatinib mesylate" ("orch" "phsu")))
  (8 (385728 "CGP 57148" ("phsu" "orch")))
  (63 (906802 "STI571" ("phsu" "orch")))
  (55 (935987 "Gleevec" ("orch" "phsu"))))

;; The right way to do it
;; Notice only 3 of the 5 answers are specific genes.
(map
  (lambda (drug)
    (let ((e-drug/genes (run* (q)
                          (fresh (gene e-drug/gene p-drug/gene p-st-drug/gene e-drug/gene-rest)
                            (== e-drug/gene q)
                            (== `(,drug ,gene ,p-drug/gene ,p-st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
                            (== "INHIBITS" p-drug/gene)
                            (edgeo e-drug/gene)))))
      e-drug/genes))
  '((385728 "CGP 57148" ("phsu" "orch"))))
'((((385728 "CGP 57148" ("phsu" "orch"))
    (31727 "Phosphotransferases" ("aapp" "gngm" "enzy"))
    "INHIBITS"
    "orch"
    "gngm"
    (11680792 10979973 10815921 9389713 9345054))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (33640 "PROTEIN KINASE" ("gngm" "enzy" "aapp"))
    "INHIBITS"
    "orch"
    "gngm"
    (9446752))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (915156 "Ephrin Receptor EphA8" ("gngm" "enzy" "aapp"))
    "INHIBITS"
    "orch"
    "gngm"
    (10815921))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (1417708 "NFKB1 gene" ("bacs" "aapp" "imft" "gngm"))
    "INHIBITS"
    "orch"
    "gngm"
    (10979973))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (1439337 "tyrosine kinase ABL1" ("aapp" "gngm" "enzy"))
    "INHIBITS"
    "orch"
    "gngm"
    (10200527 9389713))))

;; The wrong way to do it!  Notice that some of the edges include 'aapp' as their object type.
(map
  (lambda (drug)
    (let ((e-drug/genes (run* (q)
                          (fresh (gene e-drug/gene p-drug/gene e-drug/gene-rest)                
                            (== e-drug/gene q)
                            (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                            (== "INHIBITS" p-drug/gene)
                            (edgeo e-drug/gene)
                            (fresh (cui name concept-type*)
                              (== `(,cui ,name ,concept-type*) gene)
                              (membero "gngm" concept-type*))))))
      e-drug/genes))
  '((385728 "CGP 57148" ("phsu" "orch"))))
'((((385728 "CGP 57148" ("phsu" "orch"))
    (31727 "Phosphotransferases" ("aapp" "gngm" "enzy"))
    "INHIBITS"
    "orch"
    "gngm"
    (11680792 10979973 10815921 9389713 9345054))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (33640 "PROTEIN KINASE" ("gngm" "enzy" "aapp"))
    "INHIBITS"
    "orch"
    "gngm"
    (9446752))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (71253 "Platelet-Derived Growth Factor Receptor" ("aapp" "gngm" "enzy"))
    "INHIBITS"
    "orch"
    "aapp"
    (9389713))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (164786 "Proto-Oncogene Proteins c-akt" ("gngm" "aapp" "enzy"))
    "INHIBITS"
    "orch"
    "aapp"
    (10979973))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (915156 "Ephrin Receptor EphA8" ("gngm" "enzy" "aapp"))
    "INHIBITS"
    "orch"
    "gngm"
    (10815921))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (812228 "AKT1 gene" ("aapp" "phsu" "enzy" "gngm" "bacs"))
    "INHIBITS"
    "orch"
    "aapp"
    (10979973))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (1439337 "tyrosine kinase ABL1" ("aapp" "gngm" "enzy"))
    "INHIBITS"
    "orch"
    "gngm"
    (10200527 9389713))
   ((385728 "CGP 57148" ("phsu" "orch"))
    (1417708 "NFKB1 gene" ("bacs" "aapp" "imft" "gngm"))
    "INHIBITS"
    "orch"
    "gngm"
    (10979973))))

















;; these entries seem very similar (based on alias and synonym information)
;; If the user asks for 'imatinib' in a query, may want to look for all of these,
;; and perhaps in most-specific to least-specific order.
(935989 "imatinib" ("phsu" "orch"))
(939537 "Imatinib mesylate" ("orch" "phsu"))
(385728 "CGP 57148" ("phsu" "orch"))
(906802 "STI571" ("phsu" "orch"))
(935987 "Gleevec" ("orch" "phsu"))

;; how many genes do each of the imatinib synonyms INHIBIT?
(map
  (lambda (drug)
    (let ((genes (run* (q)
                   (fresh (gene e-drug/gene p-drug/gene e-drug/gene-rest)                
                     (== gene q)                
                     (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                     (== "INHIBITS" p-drug/gene)
                     (edgeo e-drug/gene)
                     (fresh (cui name concept-type*)
                       (== `(,cui ,name ,concept-type*) gene)
                       (membero "gngm" concept-type*))))))
      (list (length genes) drug)))
  '((935989 "imatinib" ("phsu" "orch"))
    (939537 "Imatinib mesylate" ("orch" "phsu"))
    (385728 "CGP 57148" ("phsu" "orch"))
    (906802 "STI571" ("phsu" "orch"))
    (935987 "Gleevec" ("orch" "phsu"))))
=>
'((219 (935989 "imatinib" ("phsu" "orch")))
  (90 (939537 "Imatinib mesylate" ("orch" "phsu")))
  (8 (385728 "CGP 57148" ("phsu" "orch")))
  (63 (906802 "STI571" ("phsu" "orch")))
  (55 (935987 "Gleevec" ("orch" "phsu"))))

;; a few of the genes for some of the drugs are dups
(map
  (lambda (drug)
    (let ((genes (run* (q)
                   (fresh (gene e-drug/gene p-drug/gene e-drug/gene-rest)                
                     (== gene q)                
                     (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                     (== "INHIBITS" p-drug/gene)
                     (edgeo e-drug/gene)
                     (fresh (cui name concept-type*)
                       (== `(,cui ,name ,concept-type*) gene)
                       (membero "gngm" concept-type*))))))
      (list (length (rem-dups genes)) drug)))
  '((935989 "imatinib" ("phsu" "orch"))
    (939537 "Imatinib mesylate" ("orch" "phsu"))
    (385728 "CGP 57148" ("phsu" "orch"))
    (906802 "STI571" ("phsu" "orch"))
    (935987 "Gleevec" ("orch" "phsu"))))
=>
'((206 (935989 "imatinib" ("phsu" "orch")))
  (86 (939537 "Imatinib mesylate" ("orch" "phsu")))
  (8 (385728 "CGP 57148" ("phsu" "orch")))
  (61 (906802 "STI571" ("phsu" "orch")))
  (52 (935987 "Gleevec" ("orch" "phsu"))))

(920288 "C-KIT Gene" ("gngm" "aapp"))
(1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))

;; every drug other than "CGP 57148" INHIBITS both KIT and C-KIT
(map
  (lambda (drug)
    (let ((genes (run* (q)
                   (fresh (gene e-drug/gene p-drug/gene e-drug/gene-rest)                
                     (== gene q)                
                     (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                     (== "INHIBITS" p-drug/gene)
                     (edgeo e-drug/gene)
                     (fresh (cui name concept-type*)
                       (== `(,cui ,name ,concept-type*) gene)
                       (membero "gngm" concept-type*))))))
      (let ((genes (rem-dups genes)))
        (list (length genes)
              (and (member '(1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp")) genes) #t)
              (and (member '(920288 "C-KIT Gene" ("gngm" "aapp")) genes) #t)
              drug))))
  '((935989 "imatinib" ("phsu" "orch"))
    (939537 "Imatinib mesylate" ("orch" "phsu"))
    (385728 "CGP 57148" ("phsu" "orch"))
    (906802 "STI571" ("phsu" "orch"))
    (935987 "Gleevec" ("orch" "phsu"))))
=>
'((206 #t #t (935989 "imatinib" ("phsu" "orch")))
  (86 #t #t (939537 "Imatinib mesylate" ("orch" "phsu")))
  (8 #f #f (385728 "CGP 57148" ("phsu" "orch")))
  (61 #t #t (906802 "STI571" ("phsu" "orch")))
  (52 #t #t (935987 "Gleevec" ("orch" "phsu"))))


;; 286 genes combined across all synonyms
(let ((all-genes
       (map
        (lambda (drug)
          (let ((genes (run* (q)
                         (fresh (gene e-drug/gene p-drug/gene e-drug/gene-rest)                
                           (== gene q)                
                           (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                           (== "INHIBITS" p-drug/gene)
                           (edgeo e-drug/gene)
                           (fresh (cui name concept-type*)
                             (== `(,cui ,name ,concept-type*) gene)
                             (membero "gngm" concept-type*))))))
            (rem-dups genes)))
        '((935989 "imatinib" ("phsu" "orch"))
          (939537 "Imatinib mesylate" ("orch" "phsu"))
          (385728 "CGP 57148" ("phsu" "orch"))
          (906802 "STI571" ("phsu" "orch"))
          (935987 "Gleevec" ("orch" "phsu"))))))
  (length (apply union* all-genes)))
=>
286

;; Genes inhibited by Gleevec
;;
;; Of the 52 results, at least a few appear to be classes of genes
;; rather than specific genes:
;;
;; (3241 "Antibodies" ("gngm" "aapp" "imft"))
;; (4891 "Fusion Proteins, bcr-abl" ("aapp" "gngm" "bacs"))
;; (31727 "Phosphotransferases" ("aapp" "gngm" "enzy"))
;; (33640 "PROTEIN KINASE" ("gngm" "enzy" "aapp"))
;; (33684 "Proteins" ("bacs" "gngm" "aapp"))
;; (79050 "c-abl Proto-Oncogenes" ("aapp" "gngm"))
;; (79413 "Genes, abl" ("gngm" "aapp"))
;; (79427 "Tumor Suppressor Genes" ("gngm" "aapp"))
;; (80298 "v-src Oncogenes" ("gngm" "aapp" "enzy" "aapp" "gngm" "bacs"))
;; (597357 "receptor" ("aapp" "gngm" "rcpt"))
;; (1136340 "Semaphorins" ("bacs" "gngm" "aapp"))
;;
;; Of course, is something like (33684 "Proteins" ("bacs" "gngm" "aapp"))
;; even considered a gene class?
;;
;; Oh wow--I had missed the first one of the c-kit names before!
;; So there are actually three synonyms, not two.
;;
;; (72470 "Proto-Oncogene Protein c-kit" ("aapp" "gngm" "rcpt" "imft"))
;; (920288 "C-KIT Gene" ("gngm" "aapp"))
;; (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
;;
;; So, of the 52 results, ~10 are actually categories rather
;; than specific genes.  Of the remaining ~40 specific genes, 3 of
;; them are synonyms/aliases for KIT.
(time (rem-dups
       (run* (q)
         (fresh (drug gene e-drug/gene p-drug/gene e-drug/gene-rest)          
           (== gene q)          
           (== '(935987 "Gleevec" ("orch" "phsu")) drug)
           (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
           (== "INHIBITS" p-drug/gene)
           (edgeo e-drug/gene)
           (fresh (cui name concept-type*)
             (== `(,cui ,name ,concept-type*) gene)
             (membero "gngm" concept-type*))))))

;; I think these two are *classes* of genes, not specific genes
;;
;; (31727 "Phosphotransferases" ("aapp" "gngm" "enzy"))
;; (33640 "PROTEIN KINASE" ("gngm" "enzy" "aapp"))
;;
;; notice there is no KIT or C-KIT
(time (run* (q)
        (fresh (drug gene e-drug/gene p-drug/gene e-drug/gene-rest)          
          (== gene q)          
          (== '(385728 "CGP 57148" ("phsu" "orch")) drug)
          (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
          (== "INHIBITS" p-drug/gene)
          (edgeo e-drug/gene)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) gene)
            (membero "gngm" concept-type*)))))
=>
'((31727 "Phosphotransferases" ("aapp" "gngm" "enzy"))
  (33640 "PROTEIN KINASE" ("gngm" "enzy" "aapp"))
  (71253 "Platelet-Derived Growth Factor Receptor" ("aapp" "gngm" "enzy"))
  (164786 "Proto-Oncogene Proteins c-akt" ("gngm" "aapp" "enzy"))
  (915156 "Ephrin Receptor EphA8" ("gngm" "enzy" "aapp"))
  (812228 "AKT1 gene" ("aapp" "phsu" "enzy" "gngm" "bacs"))
  (1439337 "tyrosine kinase ABL1" ("aapp" "gngm" "enzy"))
  (1417708 "NFKB1 gene" ("bacs" "aapp" "imft" "gngm")))



(time (run 1 (q)
        (fresh (drug gene e-drug/gene p-drug/gene e-drug/gene-rest)          
          (== e-drug/gene q)          
          (== '(935989 "imatinib" ("phsu" "orch")) drug)
          (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
          (== "INHIBITS" p-drug/gene)
          (edgeo e-drug/gene)
          (fresh (cui name concept-type*)
            (== `(,cui ,name ,concept-type*) gene)
            (membero "gngm" concept-type*)))))



;; https://pubchem.ncbi.nlm.nih.gov/compound/Imatinib_mesylate#section=Synonyms

;; MeSH Entry Terms
;;
;; alpha-(4-methyl-1-piperazinyl)-3'-((4-(3-pyridyl)-2-pyrimidinyl)amino)-p-tolu-p-toluidide
;; CGP 57148
;; CGP-57148
;; CGP57148
;; CGP57148B
;; Gleevec
;; Glivec
;; imatinib
;; imatinib mesylate
;; imatinib methanesulfonate
;; Mesylate, Imatinib
;; Methanesulfonate, Imatinib
;; ST 1571
;; ST1571
;; STI 571
;; STI-571
;; STI571

;; https://pubchem.ncbi.nlm.nih.gov/compound/5291#section=MeSH-Entry-Terms

;; Depositor-Supplied Synonyms
;;
;; Imatinib
;; 152459-95-5
;; sti-571
;; STI571
;; Cgp 57148
;; STI 571
;; Imatinib [INN:BAN]
;; N-(4-Methyl-3-((4-(pyridin-3-yl)pyrimidin-2-yl)amino)phenyl)-4-((4-methylpiperazin-1-yl)methyl)benzamide
;; Imatinib free base
;; UNII-BKJ8M8G5HI
;; CCRIS 9076
;; Imatinib (STI571)
;; CHEMBL941
;; 1iep
;; 1xbb
;; CGP-57148
;; Imatinib (INN)
;; Glamox (TN)
;; CHEBI:45783
;; CGP 57148B
;; Imatinib Methansulfonate
;; Kinome_3724
;; Imatinib base(IMA-3)
;; NSC743414
;; STK617705
;; BKJ8M8G5HI
;; AC1L1K0Z
;; BIDD:GT0047
;; 4-(4-METHYL-PIPERAZIN-1-YLMETHYL)-N-[4-METHYL-3-(4-PYRIDIN-3-YL-PYRIMIDIN-2-YLAMINO)-PHENYL]-BENZAMIDE

;; https://pubchem.ncbi.nlm.nih.gov/compound/Imatinib_mesylate#section=Related-Records
;;
;; related compounds
;;
;; Same Connectivity 	3 records
;; Same Parent, Connectivity 	115 records
;; Same Parent, Exact 	89 records
;; Mixtures, Components, and Neutralized Forms 	2 records
;; Similar Compounds 	416 records

> (time (pretty-print (run* (s) (fuzzy-concepto "imatinib" s))))
'((935989 "imatinib" ("phsu" "orch"))
  (939537 "Imatinib mesylate" ("orch" "phsu"))
  (1127612 "imatinib 100 MG" ("clnd"))
  (1329083 "imatinib 100 MG Oral Tablet" ("clnd"))
  (1331284 "imatinib 400 MG" ("clnd")))
cpu time: 93 real time: 94 gc time: 1

> (time (pretty-print (run* (s) (fuzzy-concepto "Gleevec" s))))
'((935987 "Gleevec" ("orch" "phsu")))
cpu time: 94 real time: 96 gc time: 0

> (time (pretty-print (run* (s) (fuzzy-concepto "CGP 57148" s))))
'((385728 "CGP 57148" ("phsu" "orch")))
cpu time: 93 real time: 95 gc time: 0

> (time (pretty-print (run* (s) (fuzzy-concepto "57148" s))))
'((385728 "CGP 57148" ("phsu" "orch")))
cpu time: 105 real time: 106 gc time: 1

> (time (pretty-print (run* (s) (fuzzy-concepto "1571" s))))
'((760931 "LY 315712" ("phsu"))
  (964110 "UK 157147" ("orch"))
  (1097576 "ST 1571" ("orch" "phsu"))
  (1175579 "GPI 15715" ("orch")))
cpu time: 109 real time: 112 gc time: 1




;; playing with ISA
;; ??? ISA Protein-tyrosine kinase inhibitor
;;
;; cpu time: 4 real time: 10 gc time: 0
'((13227 "Pharmaceutical Preparations" ("phsu"))
  (13982 "Emodin" ("orch" "bacs" "phsu"))
  (53622 "biochanin A" ("phsu" "orch"))
  (57090 "daidzein" ("orch" "vita"))
  (61202 "Genistein" ("phsu" "orch" "bacs"))
  (61202 "Genistein" ("phsu" "orch" "bacs"))
  (61202 "Genistein" ("phsu" "orch" "bacs"))
  (64695 "lavendustin A" ("phsu" "orch"))
  (83406 "methyl 2,5-dihydroxycinnamate" ("orch" "phsu"))
  (207800 "monorden" ("phsu" "orch"))
  (212399 "damnacanthal" ("phsu" "orch"))
  (213654 "tyrphostin 25" ("orch" "phsu"))
  (213997 "lavendustin C6" ("orch" "phsu"))
  (253468 "tyrphostin A46" ("orch" "phsu"))
  (258114 "PD 153035" ("phsu" "orch"))
  (295108 "lavendustin B" ("phsu" "orch"))
  (381676 "tyrphostin B46" ("orch" "phsu"))
  (384553 "tyrphostin A47" ("phsu" "orch"))
  (385728 "CGP 57148" ("phsu" "orch"))
  (528985 "tyrphostin AG 1478" ("orch" "phsu"))
  (538431 "SU 5402" ("phsu" "orch"))
  (539578 "tyrphostin AG 1296" ("orch" "phsu"))
  (638102 "tyrphostin A23" ("orch" "phsu"))
  (663164 "tyrphostin B42" ("phsu"))
  (758539 "tyrphostin A51" ("orch" "phsu"))
  (879396 "ZD1839" ("orch" "phsu"))
  (906802 "STI571" ("phsu" "orch"))
  (912413 "PTK 787" ("phsu" "orch"))
  (913199 "tyrphostin A9" ("orch" "phsu"))
  (919281 "Iressa" ("orch" "phsu"))
  (935987 "Gleevec" ("orch" "phsu"))
  (935989 "imatinib" ("phsu" "orch"))
  (939537 "Imatinib mesylate" ("orch" "phsu"))
  (1122962 "gefitinib" ("phsu" "orch"))
  (1135135 "erlotinib" ("phsu" "orch"))
  (1135136 "Tarceva" ("orch" "phsu"))
  (1135137 "OSI-774" ("orch" "phsu"))
  (1176007 "3,3',4,5'-tetrahydroxystilbene" ("orch" "phsu"))
  (1176021 "SU 11248" ("orch" "phsu"))
  (1412731 "BAG3 gene" ("gngm" "enzy" "aapp"))
  (1511179 "Bis-Tyrphostin" ("aapp"))
  (1519728
   "2-amino-4-(4'-hydroxyphenyl)-1,1,3-tricyanobuta-1,3-diene"
   ("aapp"))
  (1533491 "Erlotinib Hydrochloride" ("phsu" "orch"))
  (1570599 "AG 112" ("orch")))
(time
  (run* (q)
      (fresh (drug what-is-it e-drug/what st-drug/what ot-drug/what e-drug/what-rest)        
        (== drug q)
        (== '(1268567 "Protein-tyrosine kinase inhibitor" ("phsu")) what-is-it)
        (== `(,drug ,what-is-it "ISA" ,st-drug/what ,ot-drug/what . ,e-drug/what-rest) e-drug/what)
        (edgeo e-drug/what))))

;; playing with ISA
;; Gleevec ISA ???
;;
;; These answers seem mostly reasonable.
;; Of course there is more structure/a richer relationship between these answers than may be apparent.  Different levels of hierarchy.
'((3392 "Antineoplastic Agents" ("phsu"))
  (13216 "Pharmacotherapy" ("topp"))
  (13227 "Pharmaceutical Preparations" ("phsu"))
  (87111 "Therapeutic procedure" ("topp"))
  (243076 "antagonists" ("chvf"))
  (920425 "Cancer Treatment" ("topp"))
  (935989 "imatinib" ("phsu" "orch"))
  (939537 "Imatinib mesylate" ("orch" "phsu"))
  (1254351 "Pharmacologic Substance" ("phsu"))
  (1268567 "Protein-tyrosine kinase inhibitor" ("phsu"))
  (1449702 "Protein Kinase Inhibitors" ("phsu"))
  (1611640 "Therapeutic agent (substance)" ("phsu")))
(time
  (run* (q)
      (fresh (drug what-is-it e-drug/what st-drug/what ot-drug/what e-drug/what-rest)        
        (== what-is-it q)
        (== '(935987 "Gleevec" ("orch" "phsu")) drug)
        (== `(,drug ,what-is-it "ISA" ,st-drug/what ,ot-drug/what . ,e-drug/what-rest) e-drug/what)
        (edgeo e-drug/what))))

;; playing with ISA
;; ??? ISA Gleevec
;;
;; once again, this seems backwards.
;; 'Protein-tyrosine kinase inhibitor ISA Gleevec' seems backwards.
'((13227 "Pharmaceutical Preparations" ("phsu"))
  (1268567 "Protein-tyrosine kinase inhibitor" ("phsu")))
(time
  (run* (q)
      (fresh (what-is-it drug e-what/drug st-what/drug ot-what/drug e-what/drug-rest)        
        (== what-is-it q)
        (== '(935987 "Gleevec" ("orch" "phsu")) drug)
        (== `(,what-is-it ,drug "ISA" ,st-what/drug ,ot-what/drug . ,e-what/drug-rest) e-what/drug)
        (edgeo e-what/drug))))

;; playing with ISA
;; ??? ISA Imatinib mesylate
;;
;; Gleevec, CGP 57148, and STI571 are okay.
;; imatinib is marginal, at best.
;; The others are either backwards (Protein-tyrosine kinase inhibitor) or
;; non-sensical (Operative Surgical Procedures).
'((13227 "Pharmaceutical Preparations" ("phsu"))
  (87111 "Therapeutic procedure" ("topp"))
  (385728 "CGP 57148" ("phsu" "orch"))
  (543467 "Operative Surgical Procedures" ("topp"))
  (906802 "STI571" ("phsu" "orch"))
  (935987 "Gleevec" ("orch" "phsu"))
  (935989 "imatinib" ("phsu" "orch"))
  (1268567 "Protein-tyrosine kinase inhibitor" ("phsu"))
  (1268567 "Protein-tyrosine kinase inhibitor" ("phsu")))
(time
  (run* (q)
      (fresh (what-is-it drug e-what/drug st-what/drug ot-what/drug e-what/drug-rest)        
        (== what-is-it q)
        (== '(939537 "Imatinib mesylate" ("orch" "phsu")) drug)
        (== `(,what-is-it ,drug "ISA" ,st-what/drug ,ot-what/drug . ,e-what/drug-rest) e-what/drug)
        (edgeo e-what/drug))))

;; playing with ISA
;; ??? ISA imatinib
;;
;; Gleevec makes sense (brand name), and probably Imatinib mesylate.
;; STI571 is okay, probably. http://chemocare.com/chemotherapy/drug-info/STI-571.aspx
;; And CGP 57148.  https://www.biovision.com/imatinib-mesylate-cgp-57148b-sti-571.html
;;
;; The others seem...less good. 'Therapeutic procedure ISA imatinib' seems non-sensical.
;; 'Protein-tyrosine kinase inhibitor ISA imatinib' seems backwards.
'((3392 "Antineoplastic Agents" ("phsu"))
  (13216 "Pharmacotherapy" ("topp"))
  (13227 "Pharmaceutical Preparations" ("phsu"))
  (87111 "Therapeutic procedure" ("topp"))
  (385728 "CGP 57148" ("phsu" "orch"))
  (543467 "Operative Surgical Procedures" ("topp"))
  (906802 "STI571" ("phsu" "orch"))
  (935987 "Gleevec" ("orch" "phsu"))
  (939537 "Imatinib mesylate" ("orch" "phsu"))
  (1268567 "Protein-tyrosine kinase inhibitor" ("phsu"))
  (1268567 "Protein-tyrosine kinase inhibitor" ("phsu")))
(time
  (run* (q)
      (fresh (what-is-it drug e-what/drug st-what/drug ot-what/drug e-what/drug-rest)        
        (== what-is-it q)
        (== '(935989 "imatinib" ("phsu" "orch")) drug)
        (== `(,what-is-it ,drug "ISA" ,st-what/drug ,ot-what/drug . ,e-what/drug-rest) e-what/drug)
        (edgeo e-what/drug))))

;; playing with ISA
;; imatinib ISA ???
'((3392 "Antineoplastic Agents" ("phsu"))
  (13216 "Pharmacotherapy" ("topp"))
  (13227 "Pharmaceutical Preparations" ("phsu"))
  (13227 "Pharmaceutical Preparations" ("phsu"))
  (39796 "The science and art of healing" ("topp"))
  (87111 "Therapeutic procedure" ("topp"))
  (87111 "Therapeutic procedure" ("topp"))
  (243076 "antagonists" ("chvf"))
  (418981 "Medical therapy" ("topp"))
  (543467 "Operative Surgical Procedures" ("topp"))
  (679607 "treatment method" ("topp"))
  (920425 "Cancer Treatment" ("topp"))
  (935451 "neoplasm/cancer chemotherapy" ("topp"))
  (939537 "Imatinib mesylate" ("orch" "phsu"))
  (1254351 "Pharmacologic Substance" ("phsu"))
  (1268567 "Protein-tyrosine kinase inhibitor" ("phsu"))
  (1372955 "Active Ingredients" ("phsu"))
  (1449702 "Protein Kinase Inhibitors" ("phsu"))
  (1519313 "Signal Transduction Inhibitor" ("phsu"))
  (1533685 "Injection procedure" ("topp"))
  (1579409 "Molecular Target Inhibitors" ("phsu"))
  (1611640 "Therapeutic agent (substance)" ("phsu")))
(time
  (run* (q)
      (fresh (drug what-is-it e-drug/what st-drug/what ot-drug/what e-drug/what-rest)        
        (== what-is-it q)
        (== '(935989 "imatinib" ("phsu" "orch")) drug)
        (== `(,drug ,what-is-it "ISA" ,st-drug/what ,ot-drug/what . ,e-drug/what-rest) e-drug/what)
        (edgeo e-drug/what))))




;; let's test the individual parts of the query

;; compare
;;
;; (32 (920288 "C-KIT Gene" ("gngm" "aapp")))
;;
;; and
;;
;; (35 (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp")))
;;
;; which cause 32 and 35 diseases, respectively, with these monstrosities:
;;
;; (291 (597357 "receptor" ("aapp" "gngm" "rcpt")))
;; (296 (1823619 "VEGFA gene" ("bacs" "phsu" "rcpt" "gngm" "imft" "enzy" "aapp")))
;; (418 (1456820 "Tumor Necrosis Factor-alpha" ("imft" "gngm" "aapp")))
;; (506 (33684 "Proteins" ("bacs" "gngm" "aapp")))
;; (579 (79189 "cytokine" ("aapp" "imft" "gngm")))
;; (1171 (17337 "Genes" ("aapp" "gngm"))))
;;
;; Could either just drop entries like (1171 (17337 "Genes" ("aapp" "gngm")))
;; and (506 (33684 "Proteins" ("bacs" "gngm" "aapp"))), or prioritize search
;; to start with smallest number of diseases (or both).
;;
;; Also, should make sure to remove duplicate diseases in the results!
;; And remove ridiculous entries like (12634 "Disease" ("dsyn"))
;;
;; Also, should take the union of diseases caused by C-KIT and KIT,
;; rather than trying them both separately (and getting duplicate
;; answers).
;;
;; Should also consider taking the union of all diseases produced by
;; all genes, to avoid duplicate work, or do caching/memoization/tabling.
;;
(let ((genes-inhibited-by-imatinib
       (run* (q)
         (fresh (drug gene known-disease something unknown-disease
                      e-drug/gene st-drug/gene e-drug/gene-rest
                      e-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
                      e-drug/known-disease e-drug/known-disease-rest
                      e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
                      e-something/unknown-disease p-something/unknown-disease st-something/unknown-disease ot-something/unknown-disease e-something/unknown-disease-rest)
          
           (== gene q)

           ;; imatinib inhibits some gene
           (== '(935989 "imatinib" ("phsu" "orch")) drug)
           (== `(,drug ,gene "INHIBITS" ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
           (edgeo e-drug/gene)

           ))))
  (let ((genes-inhibited-by-imatinib (rem-dups genes-inhibited-by-imatinib)))
    (sort
      (map (lambda (gene)
             (let ((num-diseases-caused-by-gene
                    (length
                     (run* (q)
                       (fresh (drug known-disease something unknown-disease
                                    e-drug/gene st-drug/gene e-drug/gene-rest
                                    e-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
                                    e-drug/known-disease e-drug/known-disease-rest
                                    e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
                                    e-something/unknown-disease p-something/unknown-disease st-something/unknown-disease ot-something/unknown-disease e-something/unknown-disease-rest)
                       
                         (== e-gene/known-disease q)

                         ;; that gene directly causes some disease...
                         (== `(,gene ,known-disease "CAUSES" ,st-gene/known-disease ,ot-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
                         (conde
                           [(== "dsyn" ot-gene/known-disease)]
                           [(== "neop" ot-gene/known-disease)])
                         (edgeo e-gene/known-disease))))))
               (list num-diseases-caused-by-gene gene)))
           genes-inhibited-by-imatinib)
      (lambda (l1 l2) (< (car l1) (car l2))))))

;; how many genes are inhibited by imatinib?
;;
;; cpu time: 10 real time: 10 gc time: 0
;; 213 genes inhibited by imatinib
;;
;; some of these genes are dups!  why?
;; 206 unique genes
;;
;; one of these answers is (17337 "Genes" ("aapp" "gngm")), as opposed to a specific gene--does this result in a degenerate blowup?  This seems fishy!!
(time
  (length
    (run* (q)
      (fresh (drug gene known-disease something unknown-disease
                   e-drug/gene st-drug/gene e-drug/gene-rest
                   e-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
                   e-drug/known-disease e-drug/known-disease-rest
                   e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
                   e-something/unknown-disease p-something/unknown-disease st-something/unknown-disease ot-something/unknown-disease e-something/unknown-disease-rest)
          
        (== gene q)

        ;; imatinib inhibits some gene
        (== '(935989 "imatinib" ("phsu" "orch")) drug)
        (== `(,drug ,gene "INHIBITS" ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
        (edgeo e-drug/gene)

        ))))

;; how many diseases is imatinib known to treat (directly)?
;;
;; cpu time: 11 real time: 11 gc time: 0
;; 349 diseases that imatinib is known to treat
;;
;; hmmm--list contains dups!  why is that?
;;
;; only 277 of the diseases are unique--the rest are dups
;;
;; also, once again we see general categories of diseases--do they result in degenerate blowup?
;;
;; (3047 "Animal Diseases" ("dsyn"))
;; (8679 "Chronic Disease" ("dsyn"))
;; (12634 "Disease" ("dsyn"))
;; (12634 "Disease" ("dsyn"))
;; (920627 "Orphan Diseases" ("dsyn"))
;;
;; many of these diseases seen overlapping, or members of another class of disease
;;
;; (598934 "tumor growth" ("neop"))
;; (877373 "Advanced cancer" ("neop"))
;;
;; (1266119 "Solitary fibrous tumor" ("neop"))
;; (1266120 "Solitary fibrous tumor, malignant" ("neop"))
;;
;; (6118 "Brain Neoplasms" ("neop"))
;; (17636 "Glioblastoma" ("neop"))
;;
;; (17638 "Glioma" ("neop"))
;; (555198 "Malignant Glioma" ("neop"))
;; (677865 "Brain stem glioma" ("neop"))
;; (1319185 "Chiasmal glioma" ("neop"))
;;
;;
;; wut!?
;; (1524028 "Intraepithelial Neoplasia of the Mouse Mammary Gland" ("neop"))
;;
;; I'm pretty sure we don't want animal diseases, unless humans are considered animals!
(time
  (length
    (run* (q)
      (fresh (drug gene known-disease something unknown-disease
                   e-drug/gene st-drug/gene e-drug/gene-rest
                   e-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
                   e-drug/known-disease st-drug/known-disease ot-drug/known-disease e-drug/known-disease-rest
                   e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
                   e-something/unknown-disease p-something/unknown-disease st-something/unknown-disease ot-something/unknown-disease e-something/unknown-disease-rest)
          
        (== known-disease q)
    
        ;; imatinib
        (== '(935989 "imatinib" ("phsu" "orch")) drug)
        
        ;; ...which imatinib is known to treat
        (== `(,drug ,known-disease "TREATS" ,st-drug/known-disease ,ot-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)

        (conde
          [(== "dsyn" ot-drug/known-disease)]
          [(== "neop" ot-drug/known-disease)])
        
        (edgeo e-drug/known-disease)                  

        ))))

;; let's look at the duplicate entries for (12634 "Disease" ("dsyn")), treated by imatinib
;;
;; That seems a bit unfortunate!  imatinib is considered 'orch'
;; (Organic Chemical) in one entry, and 'phsu' (Pharmacologic
;; Substance) in the other.
;;
;; (((935989 "imatinib" ("phsu" "orch"))
;;   (12634 "Disease" ("dsyn"))
;;   "TREATS"
;;   "orch"
;;   "dsyn"
;;   (86095345 67097950 63793027))
;;
;;  ((935989 "imatinib" ("phsu" "orch"))
;;   (12634 "Disease" ("dsyn"))
;;   "TREATS"
;;   "phsu"
;;   "dsyn"
;;   (89407082 ... 41787008)))
;;
(run* (q)
  (fresh (drug gene known-disease something unknown-disease
               e-drug/gene st-drug/gene e-drug/gene-rest
               e-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
               e-drug/known-disease st-drug/known-disease ot-drug/known-disease e-drug/known-disease-rest
               e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
               e-something/unknown-disease p-something/unknown-disease st-something/unknown-disease ot-something/unknown-disease e-something/unknown-disease-rest)
    
    (== e-drug/known-disease q)
    
    ;; imatinib
    (== '(935989 "imatinib" ("phsu" "orch")) drug)
        
    ;; ...which imatinib is known to treat
    (== `(,drug ,known-disease "TREATS" ,st-drug/known-disease ,ot-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)

    (== '(12634 "Disease" ("dsyn")) known-disease)
    
    (conde
      [(== "dsyn" ot-drug/known-disease)]
      [(== "neop" ot-drug/known-disease)])
        
    (edgeo e-drug/known-disease)                  

    ))

;; more minor cleanup/inlining
;;
;; same speed, as expected
;; cpu time: 17898 real time: 17893 gc time: 462
(time (run 1 (q)
  (fresh (drug gene known-disease something unknown-disease
          e-drug/gene st-drug/gene e-drug/gene-rest
          e-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
          e-drug/known-disease e-drug/known-disease-rest
          e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
          e-something/unknown-disease p-something/unknown-disease st-something/unknown-disease ot-something/unknown-disease e-something/unknown-disease-rest)
          
    (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)

    ;; ** cheat **
    (fuzzy-concepto "KIT gene" gene)
    
    ;; imatinib inhibits some gene
    (== '(935989 "imatinib" ("phsu" "orch")) drug)
    (== `(,drug ,gene "INHIBITS" ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
    (edgeo e-drug/gene)

    ;; that gene directly causes some disease...
    (== `(,gene ,known-disease "CAUSES" ,st-gene/known-disease ,ot-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
    (conde
      [(== "dsyn" ot-gene/known-disease)]
      [(== "neop" ot-gene/known-disease)])
    (edgeo e-gene/known-disease)

    ;; ...which imatinib is known to treat
    (== `(,drug ,known-disease "TREATS" . ,e-drug/known-disease-rest) e-drug/known-disease)
    (edgeo e-drug/known-disease)                  
    
    ;; and that gene indirectly causes & indirectly affects some other disease
    (== `(,gene ,something ,p-gene/something ,st-gene/something ,ot-gene/something . ,e-gene/something-rest) e-gene/something)
    (== `(,something ,unknown-disease ,p-something/unknown-disease ,st-something/unknown-disease ,ot-something/unknown-disease . ,e-something/unknown-disease-rest) e-something/unknown-disease)
    (== "CAUSES" p-gene/something)

    (=/= "dsyn" ot-gene/something)
    (=/= "neop" ot-gene/something)
    
    (edgeo e-gene/something)

    (conde
      [(== "AFFECTS" p-something/unknown-disease)]
      [(== "CAUSES" p-something/unknown-disease)])

    (conde
      [(== "dsyn" ot-something/unknown-disease)]
      [(== "neop" ot-something/unknown-disease)])
    
    (edgeo e-something/unknown-disease)
    
    ;; generate and test!!
    (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
    (fuzzy-concepto "mast cell activation" something)
    (fuzzy-concepto "asthma" unknown-disease)

    )))

;; replace
;;
;; (== `(,drug ,gene ,p-drug/gene ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
;; (== "INHIBITS" p-drug/gene)
;;
;; with
;;
;; (== `(,drug ,gene "INHIBITS" ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
;;
;; same time, as expected
;; cpu time: 17960 real time: 17956 gc time: 431
(time (run 1 (q)
  (fresh (drug gene known-disease something unknown-disease
          e-drug/gene st-drug/gene e-drug/gene-rest
          e-gene/known-disease p-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
          e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
          e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
          e-something/unknown-disease p-something/unknown-disease st-something/unknown-disease ot-something/unknown-disease e-something/unknown-disease-rest)
          
    (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)

    ;; ** cheat **
    (fuzzy-concepto "KIT gene" gene)
    
    ;; imatinib inhibits some gene
    (== '(935989 "imatinib" ("phsu" "orch")) drug)
    (== `(,drug ,gene "INHIBITS" ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
    (edgeo e-drug/gene)

    ;; that gene directly causes some disease...
    (== `(,gene ,known-disease ,p-gene/known-disease ,st-gene/known-disease ,ot-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
    (== "CAUSES" p-gene/known-disease)
    (conde
      [(== "dsyn" ot-gene/known-disease)]
      [(== "neop" ot-gene/known-disease)])
    (edgeo e-gene/known-disease)

    ;; ...which imatinib is known to treat
    (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
    (== "TREATS" p-drug/known-disease)
    (edgeo e-drug/known-disease)                  
    
    ;; and that gene indirectly causes & indirectly affects some other disease
    (== `(,gene ,something ,p-gene/something ,st-gene/something ,ot-gene/something . ,e-gene/something-rest) e-gene/something)
    (== `(,something ,unknown-disease ,p-something/unknown-disease ,st-something/unknown-disease ,ot-something/unknown-disease . ,e-something/unknown-disease-rest) e-something/unknown-disease)
    (== "CAUSES" p-gene/something)

    (=/= "dsyn" ot-gene/something)
    (=/= "neop" ot-gene/something)
    
    (edgeo e-gene/something)

    (conde
      [(== "AFFECTS" p-something/unknown-disease)]
      [(== "CAUSES" p-something/unknown-disease)])

    (conde
      [(== "dsyn" ot-something/unknown-disease)]
      [(== "neop" ot-something/unknown-disease)])
    
    (edgeo e-something/unknown-disease)
    
    ;; generate and test!!
    (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
    (fuzzy-concepto "mast cell activation" something)
    (fuzzy-concepto "asthma" unknown-disease)

    )))

;; replace (fuzzy-concepto "imatinib" drug) with (935989 "imatinib" ("phsu" "orch"))
;;
;; a bit faster
;; cpu time: 17844 real time: 17840 gc time: 297
(time (run 1 (q)
  (fresh (drug gene known-disease something unknown-disease
          e-drug/gene p-drug/gene st-drug/gene e-drug/gene-rest
          e-gene/known-disease p-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
          e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
          e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
          e-something/unknown-disease p-something/unknown-disease st-something/unknown-disease ot-something/unknown-disease e-something/unknown-disease-rest)
          
    (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)

    ;; ** cheat **
    (fuzzy-concepto "KIT gene" gene)
    
    ;; imatinib inhibits some gene
    (== '(935989 "imatinib" ("phsu" "orch")) drug)
    (== `(,drug ,gene ,p-drug/gene ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
    (== "INHIBITS" p-drug/gene)
    (edgeo e-drug/gene)

    ;; that gene directly causes some disease...
    (== `(,gene ,known-disease ,p-gene/known-disease ,st-gene/known-disease ,ot-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
    (== "CAUSES" p-gene/known-disease)
    (conde
      [(== "dsyn" ot-gene/known-disease)]
      [(== "neop" ot-gene/known-disease)])
    (edgeo e-gene/known-disease)

    ;; ...which imatinib is known to treat
    (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
    (== "TREATS" p-drug/known-disease)
    (edgeo e-drug/known-disease)                  
    
    ;; and that gene indirectly causes & indirectly affects some other disease
    (== `(,gene ,something ,p-gene/something ,st-gene/something ,ot-gene/something . ,e-gene/something-rest) e-gene/something)
    (== `(,something ,unknown-disease ,p-something/unknown-disease ,st-something/unknown-disease ,ot-something/unknown-disease . ,e-something/unknown-disease-rest) e-something/unknown-disease)
    (== "CAUSES" p-gene/something)

    (=/= "dsyn" ot-gene/something)
    (=/= "neop" ot-gene/something)
    
    (edgeo e-gene/something)

    (conde
      [(== "AFFECTS" p-something/unknown-disease)]
      [(== "CAUSES" p-something/unknown-disease)])

    (conde
      [(== "dsyn" ot-something/unknown-disease)]
      [(== "neop" ot-something/unknown-disease)])
    
    (edgeo e-something/unknown-disease)
    
    ;; generate and test!!
    (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
    (fuzzy-concepto "mast cell activation" something)
    (fuzzy-concepto "asthma" unknown-disease)

    )))

;; replaced
;;
;; (fresh (cui name concept-type*)
;;   (== `(,cui ,name ,concept-type*) unknown-disease)
;;     (conde
;;       [(membero "dsyn" concept-type*)]
;;       [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))
;;
;; with
;;
;; (conde
;;   [(== "dsyn" ot-something/unknown-disease)]
;;   [(== "neop" ot-something/unknown-disease)])
;;
;; faster!
;; cpu time: 19553 real time: 19547 gc time: 507
(time (run 1 (q)
  (fresh (drug gene known-disease something unknown-disease
          e-drug/gene p-drug/gene st-drug/gene e-drug/gene-rest
          e-gene/known-disease p-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
          e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
          e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
          e-something/unknown-disease p-something/unknown-disease st-something/unknown-disease ot-something/unknown-disease e-something/unknown-disease-rest)
          
    (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)

    ;; ** cheat **
    (fuzzy-concepto "KIT gene" gene)
    
    ;; imatinib inhibits some gene
    (fuzzy-concepto "imatinib" drug)
    (== `(,drug ,gene ,p-drug/gene ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
    (== "INHIBITS" p-drug/gene)
    (edgeo e-drug/gene)

    ;; that gene directly causes some disease...
    (== `(,gene ,known-disease ,p-gene/known-disease ,st-gene/known-disease ,ot-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
    (== "CAUSES" p-gene/known-disease)
    (conde
      [(== "dsyn" ot-gene/known-disease)]
      [(== "neop" ot-gene/known-disease)])
    (edgeo e-gene/known-disease)

    ;; ...which imatinib is known to treat
    (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
    (== "TREATS" p-drug/known-disease)
    (edgeo e-drug/known-disease)                  
    
    ;; and that gene indirectly causes & indirectly affects some other disease
    (== `(,gene ,something ,p-gene/something ,st-gene/something ,ot-gene/something . ,e-gene/something-rest) e-gene/something)
    (== `(,something ,unknown-disease ,p-something/unknown-disease ,st-something/unknown-disease ,ot-something/unknown-disease . ,e-something/unknown-disease-rest) e-something/unknown-disease)
    (== "CAUSES" p-gene/something)

    (=/= "dsyn" ot-gene/something)
    (=/= "neop" ot-gene/something)
    
    (edgeo e-gene/something)

    (conde
      [(== "AFFECTS" p-something/unknown-disease)]
      [(== "CAUSES" p-something/unknown-disease)])

    (conde
      [(== "dsyn" ot-something/unknown-disease)]
      [(== "neop" ot-something/unknown-disease)])
    
    (edgeo e-something/unknown-disease)
    
    ;; generate and test!!
    (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
    (fuzzy-concepto "mast cell activation" something)
    (fuzzy-concepto "asthma" unknown-disease)

    )))

;; pushed down conde slightly
;;
;; same time
;; cpu time: 23358 real time: 23358 gc time: 1408
(time (run 1 (q)
  (fresh (drug gene known-disease something unknown-disease
          e-drug/gene p-drug/gene st-drug/gene e-drug/gene-rest
          e-gene/known-disease p-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
          e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
          e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
          e-something/unknown-disease p-something/unknown-disease e-something/unknown-disease-rest)
          
    (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)

    ;; ** cheat **
    (fuzzy-concepto "KIT gene" gene)
    
    ;; imatinib inhibits some gene
    (fuzzy-concepto "imatinib" drug)
    (== `(,drug ,gene ,p-drug/gene ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
    (== "INHIBITS" p-drug/gene)
    (edgeo e-drug/gene)

    ;; that gene directly causes some disease...
    (== `(,gene ,known-disease ,p-gene/known-disease ,st-gene/known-disease ,ot-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
    (== "CAUSES" p-gene/known-disease)
    (conde
      [(== "dsyn" ot-gene/known-disease)]
      [(== "neop" ot-gene/known-disease)])
    (edgeo e-gene/known-disease)

    ;; ...which imatinib is known to treat
    (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
    (== "TREATS" p-drug/known-disease)
    (edgeo e-drug/known-disease)                  
    
    ;; and that gene indirectly causes & indirectly affects some other disease
    (== `(,gene ,something ,p-gene/something ,st-gene/something ,ot-gene/something . ,e-gene/something-rest) e-gene/something)
    (== `(,something ,unknown-disease ,p-something/unknown-disease . ,e-something/unknown-disease-rest) e-something/unknown-disease)
    (== "CAUSES" p-gene/something)

    (=/= "dsyn" ot-gene/something)
    (=/= "neop" ot-gene/something)
       
    (edgeo e-gene/something)

    (conde
      [(== "AFFECTS" p-something/unknown-disease)]
      [(== "CAUSES" p-something/unknown-disease)])
    
    (edgeo e-something/unknown-disease)
                                    
    (fresh (cui name concept-type*)
      (== `(,cui ,name ,concept-type*) unknown-disease)
      (conde
        [(membero "dsyn" concept-type*)]
        [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))

    ;; generate and test!!
    (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
    (fuzzy-concepto "mast cell activation" something)
    (fuzzy-concepto "asthma" unknown-disease)

    )))

;; moved disequality constraints before conde
;;
;; same speed
;; cpu time: 23023 real time: 23015 gc time: 1251
(time (run 1 (q)
  (fresh (drug gene known-disease something unknown-disease
          e-drug/gene p-drug/gene st-drug/gene e-drug/gene-rest
          e-gene/known-disease p-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
          e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
          e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
          e-something/unknown-disease p-something/unknown-disease e-something/unknown-disease-rest)
          
    (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)

    ;; ** cheat **
    (fuzzy-concepto "KIT gene" gene)
    
    ;; imatinib inhibits some gene
    (fuzzy-concepto "imatinib" drug)
    (== `(,drug ,gene ,p-drug/gene ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
    (== "INHIBITS" p-drug/gene)
    (edgeo e-drug/gene)

    ;; that gene directly causes some disease...
    (== `(,gene ,known-disease ,p-gene/known-disease ,st-gene/known-disease ,ot-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
    (== "CAUSES" p-gene/known-disease)
    (conde
      [(== "dsyn" ot-gene/known-disease)]
      [(== "neop" ot-gene/known-disease)])
    (edgeo e-gene/known-disease)

    ;; ...which imatinib is known to treat
    (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
    (== "TREATS" p-drug/known-disease)
    (edgeo e-drug/known-disease)                  
    
    ;; and that gene indirectly causes & indirectly affects some other disease
    (== `(,gene ,something ,p-gene/something ,st-gene/something ,ot-gene/something . ,e-gene/something-rest) e-gene/something)
    (== `(,something ,unknown-disease ,p-something/unknown-disease . ,e-something/unknown-disease-rest) e-something/unknown-disease)
    (== "CAUSES" p-gene/something)

    (=/= "dsyn" ot-gene/something)
    (=/= "neop" ot-gene/something)
    
    (conde
      [(== "AFFECTS" p-something/unknown-disease)]
      [(== "CAUSES" p-something/unknown-disease)])
   
    (edgeo e-gene/something)
                  
    (edgeo e-something/unknown-disease)
                                    
    (fresh (cui name concept-type*)
      (== `(,cui ,name ,concept-type*) unknown-disease)
      (conde
        [(membero "dsyn" concept-type*)]
        [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))

    ;; generate and test!!
    (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
    (fuzzy-concepto "mast cell activation" something)
    (fuzzy-concepto "asthma" unknown-disease)

    )))

;; removed
;;
;; (fresh (cui name concept-type*)
;;   (== `(,cui ,name ,concept-type*) something)
;;   (not-membero "dsyn" concept-type*)
;;   (not-membero "neop" concept-type*))
;;
;; no slower, but no faster
;; cpu time: 23124 real time: 23118 gc time: 1233
(time (run 1 (q)
  (fresh (drug gene known-disease something unknown-disease
          e-drug/gene p-drug/gene st-drug/gene e-drug/gene-rest
          e-gene/known-disease p-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
          e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
          e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
          e-something/unknown-disease p-something/unknown-disease e-something/unknown-disease-rest)
          
    (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)

    ;; ** cheat **
    (fuzzy-concepto "KIT gene" gene)
          
    ;; imatinib inhibits some gene
    (fuzzy-concepto "imatinib" drug)
    (== `(,drug ,gene ,p-drug/gene ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
    (== "INHIBITS" p-drug/gene)
    (edgeo e-drug/gene)

    ;; that gene directly causes some disease...
    (== `(,gene ,known-disease ,p-gene/known-disease ,st-gene/known-disease ,ot-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
    (== "CAUSES" p-gene/known-disease)
    (conde
      [(== "dsyn" ot-gene/known-disease)]
      [(== "neop" ot-gene/known-disease)])
    (edgeo e-gene/known-disease)

    ;; ...which imatinib is known to treat
    (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
    (== "TREATS" p-drug/known-disease)
    (edgeo e-drug/known-disease)                  
    
    ;; and that gene indirectly causes & indirectly affects some other disease
    (== `(,gene ,something ,p-gene/something ,st-gene/something ,ot-gene/something . ,e-gene/something-rest) e-gene/something)
    (== `(,something ,unknown-disease ,p-something/unknown-disease . ,e-something/unknown-disease-rest) e-something/unknown-disease)
    (== "CAUSES" p-gene/something)
    (conde
      [(== "AFFECTS" p-something/unknown-disease)]
      [(== "CAUSES" p-something/unknown-disease)])

    (=/= "dsyn" ot-gene/something)
    (=/= "neop" ot-gene/something)
   
    (edgeo e-gene/something)
                  
    (edgeo e-something/unknown-disease)
                                    
    (fresh (cui name concept-type*)
      (== `(,cui ,name ,concept-type*) unknown-disease)
      (conde
        [(membero "dsyn" concept-type*)]
        [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))

    ;; generate and test!!
    (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
    (fuzzy-concepto "mast cell activation" something)
    (fuzzy-concepto "asthma" unknown-disease)

    )))

;; added
;;
;; (=/= "dsyn" ot-gene/something)
;; (=/= "neop" ot-gene/something)
;;
;; same time
;; cpu time: 23524 real time: 23522 gc time: 1462
(time (run 1 (q)
  (fresh (drug gene known-disease something unknown-disease
          e-drug/gene p-drug/gene st-drug/gene e-drug/gene-rest
          e-gene/known-disease p-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
          e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
          e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
          e-something/unknown-disease p-something/unknown-disease e-something/unknown-disease-rest)
          
    (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)

    ;; ** cheat **
    (fuzzy-concepto "KIT gene" gene)
          
    ;; imatinib inhibits some gene
    (fuzzy-concepto "imatinib" drug)
    (== `(,drug ,gene ,p-drug/gene ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
    (== "INHIBITS" p-drug/gene)
    (edgeo e-drug/gene)

    ;; that gene directly causes some disease...
    (== `(,gene ,known-disease ,p-gene/known-disease ,st-gene/known-disease ,ot-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
    (== "CAUSES" p-gene/known-disease)
    (conde
      [(== "dsyn" ot-gene/known-disease)]
      [(== "neop" ot-gene/known-disease)])
    (edgeo e-gene/known-disease)

    ;; ...which imatinib is known to treat
    (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
    (== "TREATS" p-drug/known-disease)
    (edgeo e-drug/known-disease)                  
    
    ;; and that gene indirectly causes & indirectly affects some other disease
    (== `(,gene ,something ,p-gene/something ,st-gene/something ,ot-gene/something . ,e-gene/something-rest) e-gene/something)
    (== `(,something ,unknown-disease ,p-something/unknown-disease . ,e-something/unknown-disease-rest) e-something/unknown-disease)
    (== "CAUSES" p-gene/something)
    (conde
      [(== "AFFECTS" p-something/unknown-disease)]
      [(== "CAUSES" p-something/unknown-disease)])

    (=/= "dsyn" ot-gene/something)
    (=/= "neop" ot-gene/something)
   
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
    (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
    (fuzzy-concepto "mast cell activation" something)
    (fuzzy-concepto "asthma" unknown-disease)

    )))

;; baby step!
;;
;; added st-gene/something ot-gene/something
;;
;; time is the same
;; cpu time: 23438 real time: 23430 gc time: 1282
(time (run 1 (q)
  (fresh (drug gene known-disease something unknown-disease
          e-drug/gene p-drug/gene st-drug/gene e-drug/gene-rest
          e-gene/known-disease p-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
          e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
          e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
          e-something/unknown-disease p-something/unknown-disease e-something/unknown-disease-rest)
          
    (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)

    ;; ** cheat **
    (fuzzy-concepto "KIT gene" gene)
          
    ;; imatinib inhibits some gene
    (fuzzy-concepto "imatinib" drug)
    (== `(,drug ,gene ,p-drug/gene ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
    (== "INHIBITS" p-drug/gene)
    (edgeo e-drug/gene)

    ;; that gene directly causes some disease...
    (== `(,gene ,known-disease ,p-gene/known-disease ,st-gene/known-disease ,ot-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
    (== "CAUSES" p-gene/known-disease)
    (conde
      [(== "dsyn" ot-gene/known-disease)]
      [(== "neop" ot-gene/known-disease)])
    (edgeo e-gene/known-disease)

    ;; ...which imatinib is known to treat
    (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
    (== "TREATS" p-drug/known-disease)
    (edgeo e-drug/known-disease)                  
    
    ;; and that gene indirectly causes & indirectly affects some other disease
    (== `(,gene ,something ,p-gene/something ,st-gene/something ,ot-gene/something . ,e-gene/something-rest) e-gene/something)
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

    ;; generate and test!!
    (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
    (fuzzy-concepto "mast cell activation" something)
    (fuzzy-concepto "asthma" unknown-disease)

    )))


;; replaced
;;
;; (fresh (cui name concept-type*)
;;   (== `(,cui ,name ,concept-type*) known-disease)
;;   (conde
;;     [(membero "dsyn" concept-type*)]
;;     [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))
;;
;; with
;;
;; (conde
;;   [(== "dsyn" ot-gene/known-disease)]
;;   [(== "neop" ot-gene/known-disease)])
;;
;; slight speedup
;; cpu time: 23177 real time: 23178 gc time: 1245
(time (run 1 (q)
  (fresh (drug gene known-disease something unknown-disease
          e-drug/gene p-drug/gene st-drug/gene e-drug/gene-rest
          e-gene/known-disease p-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
          e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
          e-gene/something p-gene/something e-gene/something-rest
          e-something/unknown-disease p-something/unknown-disease e-something/unknown-disease-rest)
          
    (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)

    ;; ** cheat **
    (fuzzy-concepto "KIT gene" gene)
          
    ;; imatinib inhibits some gene
    (fuzzy-concepto "imatinib" drug)
    (== `(,drug ,gene ,p-drug/gene ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
    (== "INHIBITS" p-drug/gene)
    (edgeo e-drug/gene)

    ;; that gene directly causes some disease...
    (== `(,gene ,known-disease ,p-gene/known-disease ,st-gene/known-disease ,ot-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
    (== "CAUSES" p-gene/known-disease)
    (conde
      [(== "dsyn" ot-gene/known-disease)]
      [(== "neop" ot-gene/known-disease)])
    (edgeo e-gene/known-disease)

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

    ;; generate and test!!
    (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
    (fuzzy-concepto "mast cell activation" something)
    (fuzzy-concepto "asthma" unknown-disease)

    )))


;; replaced
;;
;; (fresh (cui name concept-type*)
;;   (== `(,cui ,name ,concept-type*) gene)
;;   (membero "gngm" concept-type*))
;;
;; (== `(,drug ,gene ,p-drug/gene ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
;;
;; no faster!
;;
;; cpu time: 26518 real time: 26509 gc time: 1552
(time (run 1 (q)
  (fresh (drug gene known-disease something unknown-disease
          e-drug/gene p-drug/gene st-drug/gene e-drug/gene-rest
          e-gene/known-disease p-gene/known-disease e-gene/known-disease-rest
          e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
          e-gene/something p-gene/something e-gene/something-rest
          e-something/unknown-disease p-something/unknown-disease e-something/unknown-disease-rest)
          
    (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)

    ;; ** cheat **
    (fuzzy-concepto "KIT gene" gene)
          
    ;; imatinib inhibits some gene
    (fuzzy-concepto "imatinib" drug)
    (== `(,drug ,gene ,p-drug/gene ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
    (== "INHIBITS" p-drug/gene)
    (edgeo e-drug/gene)

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

    ;; generate and test!!
    (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
    (fuzzy-concepto "mast cell activation" something)
    (fuzzy-concepto "asthma" unknown-disease)

    )))

;; original
;; cpu time: 26492 real time: 26486 gc time: 1671
(time (run 1 (q)
  (fresh (drug gene known-disease something unknown-disease
          e-drug/gene p-drug/gene e-drug/gene-rest
          e-gene/known-disease p-gene/known-disease e-gene/known-disease-rest
          e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
          e-gene/something p-gene/something e-gene/something-rest
          e-something/unknown-disease p-something/unknown-disease e-something/unknown-disease-rest)
          
    (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)

    ;; ** cheat **
    (fuzzy-concepto "KIT gene" gene)
          
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

    ;; generate and test!!
    (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
    (fuzzy-concepto "mast cell activation" something)
    (fuzzy-concepto "asthma" unknown-disease)

    )))












;;; messed up somehow...
(time (run 1 (q)
  (fresh (drug gene known-disease something unknown-disease
          e-drug/gene p-drug/gene st-drug/gene e-drug/gene-rest
          e-gene/known-disease p-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
          e-drug/known-disease p-drug/known-disease e-drug/known-disease-rest
          e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
          e-something/unknown-disease p-something/unknown-disease st-something/unknown-disease ot-something/unknown-disease e-something/unknown-disease-rest)
    
    (== `(,e-drug/gene ,e-gene/known-disease ,e-drug/known-disease ,e-gene/something ,e-something/unknown-disease) q)

    ;; ** cheat **
    (fuzzy-concepto "KIT gene" gene)
    
    ;; imatinib inhibits some gene
    (fuzzy-concepto "imatinib" drug)
    (== `(,drug ,gene ,p-drug/gene ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
    (== "INHIBITS" p-drug/gene)
    (edgeo e-drug/gene)

    ;; that gene directly causes some disease...
    (== `(,gene ,known-disease ,p-gene/known-disease ,st-gene/known-disease ,ot-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
    (== "CAUSES" p-gene/known-disease)
    (conde
      [(== "dsyn" ot-gene/known-disease)]
      [(== "neop" ot-gene/known-disease)])
    (edgeo e-gene/known-disease)

    ;; ...which imatinib is known to treat
    (== `(,drug ,known-disease ,p-drug/known-disease . ,e-drug/known-disease-rest) e-drug/known-disease)
    (== "TREATS" p-drug/known-disease)
    (edgeo e-drug/known-disease)                  
                   
    ;; and that gene indirectly causes & indirectly affects some other disease
    (== `(,gene ,something ,p-gene/something ,st-gene/something ,ot-gene/something . ,e-gene/something-rest) e-gene/something)
    (== `(,something ,unknown-disease ,p-something/unknown-disease ,st-something/unknown-disease ,ot-something/unknown-disease . ,e-something/unknown-disease-rest) e-something/unknown-disease)
    (== "CAUSES" p-gene/something)
    (conde
      [(== "AFFECTS" p-something/unknown-disease)]
      [(== "CAUSES" p-something/unknown-disease)])

    ;; (=/= "dsyn" ot-gene/something)
    ;; (=/= "neop" ot-gene/something)

    ;; (=/= "dsyn" st-something/unknown-disease)
    ;; (=/= "neop" st-something/unknown-disease)

    (fresh (cui name concept-type*)
      (== `(,cui ,name ,concept-type*) something)
      (not-membero "dsyn" concept-type*)
      (not-membero "neop" concept-type*))

    
    (edgeo e-gene/something)

    (edgeo e-something/unknown-disease)
                                    
    (fresh (cui name concept-type*)
      (== `(,cui ,name ,concept-type*) unknown-disease)
      (conde
        [(membero "dsyn" concept-type*)]
        [(not-membero "dsyn" concept-type*) (membero "neop" concept-type*)]))

    ;; generate and test!!
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

    ;; ** cheat **
    (fuzzy-concepto "KIT gene" gene)
          
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

    ;; generate and test!!
    (fuzzy-concepto "Gastrointestinal Stromal Tumors" known-disease)
    (fuzzy-concepto "mast cell activation" something)
    (fuzzy-concepto "asthma" unknown-disease)

    )))

;; =>
;;
;; cpu time: 26928 real time: 27177 gc time: 1541
;; '((((935989 "imatinib" ("phsu" "orch"))
;;     (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
;;     "INHIBITS"
;;     "orch"
;;     "gngm"
;;     (88094027
;;      82038640
;;      78690628
;;      78513788
;;      70397515
;;      60608992
;;      57775955
;;      56779144
;;      55866397
;;      55866394
;;      54750176
;;      54602555
;;      54524739
;;      53954674
;;      53827456
;;      53794226
;;      53155624
;;      51843305
;;      51685933
;;      50494576
;;      50287491
;;      50287227
;;      49443008
;;      48324562
;;      47259531
;;      45719202
;;      44323647
;;      44187569
;;      43969275
;;      40811261
;;      40130263
;;      35363677
;;      35363677))
;;    ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
;;     (238198 "Gastrointestinal Stromal Tumors" ("neop"))
;;     "CAUSES"
;;     "aapp"
;;     "neop"
;;     (59625558 59480455 43196727 40249098))
;;    ((935989 "imatinib" ("phsu" "orch"))
;;     (238198 "Gastrointestinal Stromal Tumors" ("neop"))
;;     "TREATS"
;;     "orch"
;;     "neop"
;;     (89468570
;;      89468569
;;      89468568
;;      89258086
;;      89258085
;;      89205362
;;      88927657
;;      88924013
;;      88642836
;;      88557822
;;      88416696
;;      88236035
;;      87703148
;;      87639342
;;      87563133
;;      87355756
;;      87277832
;;      87259474
;;      87190645
;;      87155663
;;      86726495
;;      86726307
;;      86567237
;;      86229065
;;      86172855
;;      86172792
;;      86131660
;;      86095366
;;      86035145
;;      86029120
;;      85975403
;;      85946745
;;      85712439
;;      85533589
;;      85460959
;;      85258429
;;      85258243
;;      85102393
;;      84799502
;;      84760780
;;      84600823
;;      84489940
;;      84455598
;;      84282070
;;      84199399
;;      84031770
;;      83839275
;;      83526901
;;      83164056
;;      82850336
;;      82797211
;;      82613490
;;      82405375
;;      82317721
;;      82174866
;;      81986594
;;      81966494
;;      81868093
;;      81836303
;;      81836299
;;      81821592
;;      81821568
;;      81779900
;;      81749438
;;      81749298
;;      81693422
;;      81660653
;;      81532546
;;      81446896
;;      81292173
;;      81292146
;;      81187916
;;      81096155
;;      81085475
;;      80369749
;;      80098508
;;      80091451
;;      80030026
;;      80007514
;;      80007503
;;      80007484
;;      79704637
;;      78892177
;;      78600687
;;      78533753
;;      78245623
;;      78014091
;;      77943557
;;      77943438
;;      77510203
;;      76776701
;;      76743653
;;      76743620
;;      76462669
;;      76289046
;;      76148449
;;      76070843
;;      76070756
;;      75933563
;;      75933518
;;      75933507
;;      75779308
;;      75779235
;;      75722646
;;      75678381
;;      75574053
;;      75393814
;;      75385321
;;      75385290
;;      75335029
;;      74891979
;;      74719196
;;      74658329
;;      74658173
;;      74617628
;;      74612683
;;      74510446
;;      74436697
;;      74324960
;;      74324751
;;      73941360
;;      73846632
;;      73821917
;;      73790920
;;      73636829
;;      73619092
;;      73604638
;;      73358651
;;      73358394
;;      73338468
;;      73192059
;;      72992017
;;      72991953
;;      72925441
;;      72925432
;;      72387590
;;      72235834
;;      72163957
;;      72163916
;;      72152875
;;      72006640
;;      71966871
;;      71819909
;;      71747166
;;      71634856
;;      71548303
;;      71548302
;;      71458205
;;      71302542
;;      71290202
;;      71193021
;;      71178655
;;      71047643
;;      70800725
;;      70795678
;;      70677512
;;      70592280
;;      70531661
;;      70516256
;;      70480367
;;      70407945
;;      70392775
;;      70218140
;;      70065532
;;      70064632
;;      70064228
;;      69980656
;;      69922277
;;      69798593
;;      69628693
;;      69574355
;;      69574200
;;      69326210
;;      69173266
;;      69143520
;;      69073207
;;      68990202
;;      68961183
;;      68805644
;;      68756725
;;      68695975
;;      68641330
;;      68543097
;;      68490316
;;      68490176
;;      68451659
;;      68428455
;;      68428435
;;      68128844
;;      67447723
;;      67447408
;;      67447259
;;      67420026
;;      67420012
;;      67395342
;;      67377234
;;      67353857
;;      67224404
;;      67097947
;;      67020602
;;      67020578
;;      66947994
;;      66947973
;;      66919135
;;      66907557
;;      66794112
;;      66717719
;;      66717457
;;      66700212
;;      66658747
;;      66548225
;;      66498273
;;      66492422
;;      66464075
;;      66437854
;;      66316788
;;      66259767
;;      66030713
;;      66030305
;;      66030298
;;      65997826
;;      65895363
;;      65895259
;;      65895230
;;      65847488
;;      65628134
;;      65617569
;;      65512937
;;      65498233
;;      65154906
;;      64925006
;;      64924970
;;      64812649
;;      64812611
;;      64812607
;;      64752721
;;      64721564
;;      64711818
;;      64711731
;;      64656080
;;      64624730
;;      64622704
;;      64156886
;;      63841948
;;      63817256
;;      63816481
;;      63816454
;;      63683436
;;      63571342
;;      63405183
;;      63405085
;;      63381482
;;      63331732
;;      63324444
;;      63319121
;;      63318913
;;      63316021
;;      63315694
;;      63185401
;;      63070375
;;      63054852
;;      62989453
;;      62973318
;;      62841282
;;      62830344
;;      62785280
;;      62784978
;;      62784892
;;      62784888
;;      62784843
;;      62784801
;;      62462377
;;      62428126
;;      62426332
;;      62378768
;;      62378584
;;      62330072
;;      62324461
;;      61797261
;;      61735158
;;      61548783
;;      61207268
;;      61202221
;;      60969371
;;      60969364
;;      60866909
;;      60850115
;;      60662946
;;      60621352
;;      60621253
;;      60551995
;;      60440052
;;      60428943
;;      60428934
;;      60428660
;;      60393093
;;      60392934
;;      60211135
;;      60205838
;;      60062650
;;      59973854
;;      59954856
;;      59807933
;;      59797232
;;      59683223
;;      59554449
;;      59554240
;;      59553994
;;      59553949
;;      59234656
;;      59060757
;;      58957292
;;      58911796
;;      58800894
;;      58782655
;;      58759216
;;      58758787
;;      58758746
;;      58650526
;;      58547505
;;      58547365
;;      58489364
;;      58489286
;;      58489283
;;      58489063
;;      58488102
;;      58370475
;;      58082575
;;      58036800
;;      57952675
;;      57925861
;;      57528381
;;      57461748
;;      57396831
;;      57173003
;;      56779264
;;      56779187
;;      56736216
;;      56675689
;;      56644042
;;      56635283
;;      56581542
;;      56521000
;;      56516740
;;      56305864
;;      56246713
;;      55788664
;;      55669371
;;      55504860
;;      55461710
;;      55144263
;;      54972005
;;      54899752
;;      54750165
;;      54655723
;;      54637785
;;      54185481
;;      54167702
;;      54131652
;;      54065003
;;      54064819
;;      54063323
;;      54033257
;;      53990272
;;      53844942
;;      53790979
;;      53789312
;;      53781555
;;      53770969
;;      53756949
;;      53514462
;;      53485394
;;      53408441
;;      53348483
;;      53324991
;;      53090376
;;      53035552
;;      52986131
;;      52774289
;;      52626484
;;      52626456
;;      52401003
;;      52400997
;;      52384984
;;      52383401
;;      52145581
;;      52080520
;;      52080500
;;      51966571
;;      51966533
;;      51940444
;;      51843289
;;      51785870
;;      51517762
;;      51429637
;;      51233058
;;      50980581
;;      50980537
;;      50965433
;;      50781404
;;      50721080
;;      50508999
;;      50503416
;;      50494443
;;      50443602
;;      50428979
;;      50221877
;;      50221071
;;      50146289
;;      49793801
;;      49662175
;;      49459434
;;      49235490
;;      49234707
;;      49217344
;;      49217224
;;      49188407
;;      48984520
;;      48956187
;;      48740985
;;      48710539
;;      48710399
;;      48658118
;;      48629524
;;      48629262
;;      48629096
;;      48456328
;;      48420003
;;      48222031
;;      48189351
;;      48111387
;;      48019056
;;      47581439
;;      47261164
;;      47241719
;;      47241687
;;      47038924
;;      46851427
;;      46841532
;;      46821633
;;      46603672
;;      46590734
;;      46440408
;;      46423016
;;      46090733
;;      46075210
;;      45928498
;;      45928268
;;      45878480
;;      45605920
;;      45412106
;;      45295271
;;      45253578
;;      45182073
;;      45147147
;;      45071160
;;      44966652
;;      44860680
;;      44776669
;;      44760794
;;      44677169
;;      44310963
;;      44045613
;;      44045160
;;      44013797
;;      44013731
;;      44013557
;;      44005189
;;      43500401
;;      43500051
;;      43499908
;;      43467372
;;      43412654
;;      43255365
;;      43254694
;;      43254568
;;      43065434
;;      43064602
;;      42945181
;;      42944957
;;      42932034
;;      42866660
;;      42860840
;;      42860208
;;      42859293
;;      42797774
;;      42621694
;;      42598154
;;      42593639
;;      42592914
;;      42483339
;;      42475709
;;      42473986
;;      41934602
;;      41934289
;;      41934141
;;      41824732
;;      41823994
;;      41823990
;;      41823884
;;      41823658
;;      41770028
;;      41719972
;;      41636739
;;      41636678
;;      41474558
;;      41474110
;;      41417515
;;      41357545
;;      41047174
;;      41047053
;;      40913879
;;      40913586
;;      40882088
;;      40879546
;;      40810835
;;      40670230
;;      40628283
;;      40618412
;;      40508813
;;      40264027
;;      40263635
;;      40263369
;;      40248842
;;      40213767
;;      40159841
;;      39984065
;;      39953257
;;      39841942
;;      39833972
;;      39787674
;;      39649770
;;      38920751
;;      38459993
;;      37901924
;;      36961724
;;      36754535
;;      36753213
;;      36012874
;;      35363283
;;      35363160
;;      35129115
;;      34362294
;;      33891695
;;      33891545
;;      33891150
;;      31900490
;;      29766172
;;      26570601
;;      26548765))
;;    ((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
;;     (1155074 "mast cell activation" ("celf"))
;;     "CAUSES"
;;     "gngm"
;;     "celf"
;;     (36804978))
;;    ((1155074 "mast cell activation" ("celf"))
;;     (4096 "Asthma" ("dsyn"))
;;     "AFFECTS"
;;     "celf"
;;     "dsyn"
;;     (54247735 38643255))))


















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
