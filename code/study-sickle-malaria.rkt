#lang racket
(require
  "mk-db.rkt"
  )

(displayln
  "Finished loading mk-db.rkt (which probably took longer than the query).")

(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (fuzzy-concepto "sickle" s)
                        (fuzzy-concepto "malaria" m)
                        (edgeo e)))))


;; Sometimes we just want to target a specific cui or set of cuis.
;#(37054 "Sickle Cell Trait" (#("dsyn" 1)))
;#(19043 "Sickle Hemoglobin" (#("bacs" 1) #("gngm" 1) #("aapp" 1))
;#(24530 "Malaria" (#("dsyn" 1)))

(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (cui*o s '(37054 19043))
                        (cuio m 24530)
                        (edgeo e)))))




;;; Will's explorations...

;; https://malariajournal.biomedcentral.com/articles/10.1186/1475-2875-12-317

;; Nothing here...  HbAS is the technical term for someone who is heterozygous for the sickle cell gene.
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "HbAS" s)]
                          [(fuzzy-concepto "HbAS" m)])                        
                        (edgeo e)))))

;; try this variant
;;
;; alas, this version picks up 'THBS1 gene'
;;
;; is there a way to tighten up the query slightly, or at least post-filter the results?
;;
;; answers include things like:
;;
;;  ((1415488 "HBS1L gene" (1 59))
;;   (20114 "Human" (68 62))
;;   "PART_OF"
;;   "gngm"
;;   "humn"
;;   (88838886))
;; 
;; http://www.genecards.org/cgi-bin/carddisp.pl?gene=HBS1L
;;
;; GeneCards Summary for HBS1L Gene
;;
;; HBS1L (HBS1 Like Translational GTPase) is a Protein Coding gene. Diseases associated with HBS1L include Sickle Cell Disease and Sickle Cell Anemia. Among its related pathways are Deadenylation-dependent mRNA decay and Gene Expression. GO annotations related to this gene include GTP binding and translation elongation factor activity. An important paralog of this gene is GSPT2.
;;
;; also see https://www.ncbi.nlm.nih.gov/gene/10767
;;
;; This gene encodes a member of the GTP-binding elongation factor family. It is expressed in multiple tissues with the highest expression in heart and skeletal muscle. The intergenic region of this gene and the MYB gene has been identified to be a quantitative trait locus (QTL) controlling fetal hemoglobin level, and this region influnces erythrocyte, platelet, and monocyte counts as well as erythrocyte volume and hemoglobin content. DNA polymorphisms at this region associate with fetal hemoglobin levels and pain crises in sickle cell disease. A single nucleotide polymorphism in exon 1 of this gene is significantly associated with severity in beta-thalassemia/Hemoglobin E. Multiple alternatively spliced transcript variants encoding different protein isoforms have been found for this gene. [provided by RefSeq, May 2009]
;;
;; and https://www.omim.org/entry/612450
;;
;; and http://www.uniprot.org/uniprot/Q9Y450
;; 
;; this is the first query I learned something from:  I wouldn't have known to look for HBS1L if I hadn't looked at the results from this query.  That's something, I suppose.
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "HbS" s)]
                          [(fuzzy-concepto "HbS" m)])                        
                        (edgeo e)))))

;; same as above
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "HBS1L" s)]
                          [(fuzzy-concepto "HBS1L" m)])                        
                        (edgeo e)))))

;; https://malariajournal.biomedcentral.com/articles/10.1186/1475-2875-12-317
;; 
;; HbAS and protection against malaria
;;
;; Despite the obvious deleterious nature of HbSS, it is now widely accepted that the persistence of the sickle mutation in human populations is due to the protection from malaria afforded to heterozygous individuals. Haldane first proposed the concept of a heterozygote advantage against malaria in 1949 [11]. In this seminal paper, Haldane suggested that individuals heterozygous for thalassaemia, another haemoglobinopathy, were protected against malaria.
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "THBS1" s)]
                          [(fuzzy-concepto "THBS1" m)])
                        (edgeo e)))))

;; a few entries, such as
;;
;;   ((39730 "Thalassemia" (41))
;;   (1328338 "Haemoglobin E-thalassaemia disease" (41))
;;   "COEXISTS_WITH"
;;   "dsyn"
;;   "dsyn"
;;   (87633595))
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "thalassaemia" s)]
                          [(fuzzy-concepto "thalassaemia" m)])
                        (edgeo e)))))

;; nothing
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "haemoglobinopathy" s)]
                          [(fuzzy-concepto "haemoglobinopathy" m)])
                        (edgeo e)))))

;; nothing
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "Haemoglobin disease" s)]
                          [(fuzzy-concepto "Haemoglobin disease" m)])
                        (edgeo e)))))

;; nothing
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "parasitaemia" s)]
                          [(fuzzy-concepto "parasitaemia" m)])
                        (edgeo e)))))

;; nothing
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "red blood cell polymorphism" s)]
                          [(fuzzy-concepto "red blood cell polymorphism" m)])
                        (edgeo e)))))



;; From https://malariajournal.biomedcentral.com/articles/10.1186/1475-2875-12-317
;;
;; Sickle haemoglobin, sickle cell disease and sickle cell trait
;;
;; Sickle haemoglobin (HbS) is a structural variant of normal adult haemoglobin. Adult haemoglobin (HbAA) is made up of two alpha and two beta globin chains. HbS is the result of a single point mutation (Glu → Val) on the sixth codon of the beta globin gene [9]. Homozygotes for haemoglobin S (HbSS) with two affected beta chains develop sickle cell disease, in which polymerized haemoglobin causes red blood cells to sickle and occlude blood vessels. Vaso-occlusion affects many organs and tissues, and results in high morbidity and mortality. Heterozygotes for sickle haemoglobin (HbAS) have sickle cell trait and are generally asymptomatic [10].
;;
;; nothing
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "HbAS" s)]
                          [(fuzzy-concepto "HbAS" m)])                        
                        (edgeo e)))))

;; nothing
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "sickle haemoglobin" s)]
                          [(fuzzy-concepto "sickle haemoglobin" m)])                        
                        (edgeo e)))))


;; Aliaseses for sickle cell anemia, according to http://www.genecards.org/cgi-bin/carddisp.pl?gene=HBS1L#diseases:
;;
;; drepanocytosis
;; hb sc disease
;; hb-s/hb-c disease
;; hb-ss disease without crisis
;; hemoglobin s disease without crisis
;; hemoglobin sc disease
;; sickle-cell/hb-c disease without crisis
;; hbsc disease
;; sickle cell - hemoglobin c disease
;; sickle cell-hemoglobin c disease syndrome
;; hbs disease
;; hemoglobin s disease
;; sickling disorder due to hemoglobin s
;; hemoglobin sc
;; sc disease
;; hemoglobin ss
;; sickle cell disease
;; skca
;; anemia, sickle cell
;; sickle cell trait

;; 'Antisickling Agents' and 'Sickling test' entries show up, although these don't appear critical for our query.
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "sickling" s)]
                          [(fuzzy-concepto "sickling" m)])
                        (edgeo e)))))

;; nothing
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "drepanocytosis" s)]
                          [(fuzzy-concepto "drepanocytosis" m)])
                        (edgeo e)))))

;; nothing
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "hemoglobinose S" s)]
                          [(fuzzy-concepto "hemoglobinose S" m)])
                        (edgeo e)))))

;; nothing
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "sicklemia" s)]
                          [(fuzzy-concepto "sicklemia" m)])
                        (edgeo e)))))

;; nothing
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "hbsc" s)]
                          [(fuzzy-concepto "hbsc" m)])
                        (edgeo e)))))

;; nothing
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "skca" s)]
                          [(fuzzy-concepto "skca" m)])
                        (edgeo e)))))

;; lots of results!  'Hemoglobin SS disease with crisis'
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "hemoglobin ss" s)]
                          [(fuzzy-concepto "hemoglobin ss" m)])
                        (edgeo e)))))

;; nothing
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "hemoglobin s disease" s)]
                          [(fuzzy-concepto "hemoglobin s disease" m)])
                        (edgeo e)))))


;; Ah!  There are a ton of entries here!
;;
;; There are entries under 'falciparum' that we are missing from looking for 'malaria', such as 'Plasmodium falciparum infection'.
;;
;; https://en.wikipedia.org/wiki/Plasmodium_falciparum
;;
;; Plasmodium falciparum is a unicelluar protozoan parasite of humans, and the deadliest species of Plasmodium that cause malaria in humans.[2] It is transmitted through the bite of a female Anopheles mosquito. It is responsible for roughly 50% of all malaria cases.[3] It causes the disease's most dangerous form called falciparum malaria.[4][5] It is therefore regarded as the deadliest parasite in humans, causing a conservative estimate of one million deaths every year.[6]
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "falciparum" s)]
                          [(fuzzy-concepto "falciparum" m)])                        
                        (edgeo e)))))

;; https://malariajournal.biomedcentral.com/articles/10.1186/1475-2875-12-317
;;
;; Biochemical and mechanical changes in infected HbAS red blood cells have been shown to alter disease progression. Rosette formation, which is the binding of P. falciparum-infected red blood cells to uninfected red blood cells, is thought to lead to microcirculatory obstruction in cerebral malaria [55, 56, 57, 58, 59]. Rosette formation was found to be impaired in P. falciparum-infected HbAS red blood cells under deoxygenated conditions [57]. Impaired rosette formation with HbAS red blood cells may be due to increased sickling of these cells in deoxygenated conditions [47, 48] or to reduced expression of erythrocyte surface adherence proteins [60]. Decreased rosette formation and the resulting decreased circulatory obstruction might contribute to protection against severe malaria in HbAS individuals.
;;
;; 'Rosette formation' entries!
;;
;;   ((35863 "Rosette formation" (53))
;;    (24535 "Malaria, Falciparum" (41))
;;    "COEXISTS_WITH"
;;    "fndg"
;;    "dsyn"
;;    (14312565))
;;
;;   ((24530 "Malaria" (41))
;;    (35863 "Rosette formation" (53))
;;    "COEXISTS_WITH"
;;    "dsyn"
;;    "fndg"
;;    (33906042 32437650))
;;
;; ((24534 "Malaria, Cerebral" (41))
;;    (35863 "Rosette formation" (53))
;;    "COEXISTS_WITH"
;;    "dsyn"
;;    "fndg"
;;    (25261309))
;;
;;  ((35863 "Rosette formation" (53))
;;   (858320 "Plasmodium ovale infection" (41))
;;   "COEXISTS_WITH"
;;   "fndg"
;;   "dsyn"
;;   (18527923))
;;
;; look at this one
;;
;;   ((11501 "Deoxyglucose" (25 108))
;;   (35863 "Rosette formation" (53))
;;   "INHIBITS"
;;   "phsu"
;;   "fndg"
;;   (2653977))
;;
;; and
;;
;; ((142009 "erythrocyte receptor" (116 59 1))
;;  (35863 "Rosette formation" (53))
;;  "AFFECTS"
;;  "aapp"
;;  "fndg"
;;  (12083367 6642028))
;;
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "Rosette" s)]
                          [(fuzzy-concepto "Rosette" m)])
                        (edgeo e)))))

;; a few answers.  perhaps this one?
;;
;; ((35864 "Immunocytoadherence" (78))
;;  (275518 "Acute infectious disease" (41))
;;  "DIAGNOSES"
;;  "lbpr"
;;  "dsyn"
;;  (1752119))
;;
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "cytoadherence" s)]
                          [(fuzzy-concepto "cytoadherence" m)])
                        (edgeo e)))))

;; this is a good one!
;; full of entries like this:
;;
;; ((1429928 "erythrocyte membrane protein 1, Plasmodium falciparum" (59 1 70))
;;  (1511 "Adhesions" (2 41))
;;  "ASSOCIATED_WITH"
;;  "aapp"
;;  "dsyn"
;;  (81338955 73350498 42416200 31920224))
;;
;; https://malariajournal.biomedcentral.com/articles/10.1186/1475-2875-12-317
;;
;; Reduced cytoadherence has also been implicated as a mechanism of protection in HbAS individuals. Infected red blood cells express one of a family of parasite-encoded P. falciparum erythrocyte membrane protein 1 (PfEMP-1) molecules on the erythrocyte surface, and via this protein adhere to endothelial cells in the microvasculature [61, 62, 63, 64] .This process, termed cytoadherence, enables parasites to sequester in the vasculature and avoid clearance by the spleen [64].
;;
;; ((1429928 "erythrocyte membrane protein 1, Plasmodium falciparum" (59 1 70))
;;  (1511 "Adhesions" (2 41))
;;  "AUGMENTS"
;;  "aapp"
;;  "acab"
;;  (75786625))
;;
;; ((14792 "Erythrocytes" (28))
;;  (1429928 "erythrocyte membrane protein 1, Plasmodium falciparum" (59 1 70))
;;  "LOCATION_OF"
;;  "cell"
;;  "aapp"
;;  (45103237 45103232))
;;
;; ((1429928 "erythrocyte membrane protein 1, Plasmodium falciparum" (59 1 70))
;;  (6904 "Capillary Endothelium" (128))
;;  "PART_OF"
;;  "aapp"
;;  "tisu"
;;  (27874897))
;;
;; ((1429928 "erythrocyte membrane protein 1, Plasmodium falciparum" (59 1 70))
;;  (14257 "Endothelium" (128))
;;  "PART_OF"
;;  "aapp"
;;  "tisu"
;;  (81466965 59207003))
;;
;; ((1429928 "erythrocyte membrane protein 1, Plasmodium falciparum" (59 1 70))
;;  (20964 "Immunity" (107))
;;  "AFFECTS"
;;  "aapp"
;;  "phsf"
;;  (42255905))
;;
;; ((1429928 "erythrocyte membrane protein 1, Plasmodium falciparum" (59 1 70))
;;  (21311 "Infection" (41))
;;  "CAUSES"
;;  "aapp"
;;  "dsyn"
;;  (67457041))
;;
;; ((1429928 "erythrocyte membrane protein 1, Plasmodium falciparum" (59 1 70))
;;  (24530 "Malaria" (41))
;;  "AFFECTS"
;;  "aapp"
;;  "dsyn"
;;  (36678097))
;;
;; ((1429928 "erythrocyte membrane protein 1, Plasmodium falciparum" (59 1 70))
;;  (24530 "Malaria" (41))
;;  "ASSOCIATED_WITH"
;;  "aapp"
;;  "dsyn"
;;  (89492035 87380723 87061566 78738418 74230694 54367939))
;;
;; ((1429928 "erythrocyte membrane protein 1, Plasmodium falciparum" (59 1 70))
;;  (24530 "Malaria" (41))
;;  "PREVENTS"
;;  "imft"
;;  "dsyn"
;;  (81990442))
;;
;; ((1429928 "erythrocyte membrane protein 1, Plasmodium falciparum" (59 1 70))
;;  (24534 "Malaria, Cerebral" (41))
;;  "ASSOCIATED_WITH"
;;  "aapp"
;;  "dsyn"
;;  (82747148))
;;
;; ((1429928 "erythrocyte membrane protein 1, Plasmodium falciparum" (59 1 70))
;;  (24535 "Malaria, Falciparum" (41))
;;  "CAUSES"
;;  "aapp"
;;  "dsyn"
;;  (74670901 67457034))
;;
;; ((1429928 "erythrocyte membrane protein 1, Plasmodium falciparum" (59 1 70))
;;  (24535 "Malaria, Falciparum" (41))
;;  "PREVENTS"
;;  "imft"
;;  "dsyn"
;;  (48365380 36796545))
;;
;; and look at this one!
;; 
;; ((1429928 "erythrocyte membrane protein 1, Plasmodium falciparum" (59 1 70))
;;  (1368474 "rosetting" (27))
;;  "AFFECTS"
;;  "aapp"
;;  "celf"
;;  (75067858 58808770 37978092))
;;
(time (pretty-print (run* (e)
                      (fresh (s m e-rest)
                        (== `(,s ,m . ,e-rest) e)
                        (conde
                          [(fuzzy-concepto "erythrocyte membrane protein" s)]
                          [(fuzzy-concepto "erythrocyte membrane protein" m)])
                        (edgeo e)))))





;; https://malariajournal.biomedcentral.com/articles/10.1186/1475-2875-12-317
;;
;; These discrepancies suggest that the mechanism of protection afforded by HbAS is complex, with impacts on both the development of parasitaemia and the control of parasitaemia once it is established.


;; https://malariajournal.biomedcentral.com/articles/10.1186/1475-2875-12-317
;;
;; Other related diseases to explore:
;;
;; These data provide comprehensive evidence for a global geographical association between malaria burden and HbS allele frequency, particularly in sub-Saharan Africa. Furthermore, evidence for the protective effects of other red blood cell polymorphisms against malaria, including haemoglobin C, haemoglobin E, thalassaemias, and ovalocytosis have also been described [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28].
