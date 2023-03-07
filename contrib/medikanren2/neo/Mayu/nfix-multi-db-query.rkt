#lang racket/base

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level-multi-db.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers-multi-db.rkt"
 json
 racket/format
 racket/list
 racket/match
 racket/set
 racket/pretty
 racket/string)


;; drug ->regulates-> gene ->regulates-> NFIX
(define (run-simple-2-hop)
  (query:X->Y->Known
   ;; category*.X 
   (set->list
    (get-non-deprecated-mixed-ins-and-descendent-classes*-in-db
     (list "biolink:Drug")))
   ;; predicate*.X->Y
   (set->list
    (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
     '("biolink:regulates")))
  
   ;; category*.Y
   (set->list
    (get-non-deprecated-mixed-ins-and-descendent-classes*-in-db
     (list "biolink:Gene")))
   ;; predicate*.Y->K
   (set->list
    (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
     '("biolink:regulates")))
 
   ;; curie*.K
   (set->list
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db
      (list "HGNC:7788"))))
   ))

(run-simple-2-hop)
;; =>
'(("UMLS:C0017302"
   "General anesthetic drugs"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0017302---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:26400403")
    ("publications_info"
     "{\"PMID:26400403\": {\"publication date\": \"2015 Dec\", \"sentence\": \"While all three general anesthetics effectively blocked nociceptive responses and activation of ERK in the rat ACC following formalin injection during anesthesia, only sevoflurane inhibited ERK activation in the spinal cord and ACC at 24 h post-injection.\", \"subject score\": 901, \"object score\": 1000}}")
    ("subject" "UMLS:C0017302"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0304497"
   "Cytotoxic agent (product)"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0304497---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:29507530")
    ("publications_info"
     "{\"PMID:29507530\": {\"publication date\": \"2018\", \"sentence\": \"Conclusion: Taken together, our data identified nilotinib as a cytotoxic drug that combined with ERK inhibitors deserves to be tested as a novel therapy for adrenocortical carcinoma.\", \"subject score\": 1000, \"object score\": 861}}")
    ("subject" "UMLS:C0304497"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0597220"
   "phospholipase inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0597220---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:19048108")
    ("publications_info"
     "{\"PMID:19048108\": {\"publication date\": \"2008\", \"sentence\": \"A phospholipase Cgamma (PLCgamma) inhibitor and shRNA, as well as an Erk inhibitor, reduced ST6Gal1 and FUT9 mRNA levels and inhibited effects of L1 on neurite outgrowth and cell survival.\", \"subject score\": 913, \"object score\": 827}}")
    ("subject" "UMLS:C0597220"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0040616"
   "Anti-Anxiety Agents"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0040616---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30102992")
    ("publications_info"
     "{\"PMID:30102992\": {\"publication date\": \"2018 Nov 15\", \"sentence\": \"CONCLUSION: This study demonstrated that aqueous extracts from S. japonicus viscera are effective whitening and anti-aging agents that stimulate ERK signaling to inhibit melanin synthesis and promote collagen synthesis.\", \"subject score\": 901, \"object score\": 861}}")
    ("subject" "UMLS:C0040616"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0003211"
   "Anti-Inflammatory Agents, Non-Steroidal"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003211---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:16230075")
    ("publications_info"
     "{\"PMID:16230075\": {\"publication date\": \"2005 Oct\", \"sentence\": \"Exposure to NSAIDs inhibits Sp1 and ERK phosphorylation.\", \"subject score\": 1000, \"object score\": 861}}")
    ("subject" "UMLS:C0003211"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1514713"
   "Raf Kinase Inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1514713---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:18163502")
    ("publications_info"
     "{\"PMID:18163502\": {\"publication date\": \"2008 Jan\", \"sentence\": \"Components of the ERK pathway were inhibited by Raf-1 kinase inhibitor and the MEK inhibitor, PD98059.\", \"subject score\": 923, \"object score\": 861}}")
    ("subject" "UMLS:C1514713"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0003385"
   "Muscarinic Antagonists"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003385---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:19533734")
    ("publications_info"
     "{\"PMID:19533734\": {\"publication date\": \"2009 Dec\", \"sentence\": \"These effects of fustin were reversed by treatment with dicyclomine, a muscarinic M1 receptor antagonist, and SL327, a selective ERK inhibitor, but not by chelerythrine, a pan-protein kinase C (PKC) inhibitor.\", \"subject score\": 880, \"object score\": 793}}")
    ("subject" "UMLS:C0003385"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0729502"
   "Chemotherapeutic agent"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0729502---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30288546")
    ("publications_info"
     "{\"PMID:30288546\": {\"publication date\": \"2018 Dec\", \"sentence\": \"Combining chemotherapeutic agents with HDAC inhibitor (TSA) or with targeting Raf/MEK/ERK pathway is promising to circumvent chemoresistance in UCs.\", \"subject score\": 983, \"object score\": 754}}")
    ("subject" "UMLS:C0729502"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0598695"
   "NMDA receptor antagonist"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0598695---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:19361551")
    ("publications_info"
     "{\"PMID:19361551\": {\"publication date\": \"2009 Aug\", \"sentence\": \"ERK activation could indicate a stress-mediated increase in glutamatergic signaling, therefore mice were treated prior to SNI and stress with memantine, an N-methyl-D-aspartate receptor (NMDAR) antagonist.\", \"subject score\": 954, \"object score\": 1000}}")
    ("subject" "UMLS:C0598695"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1136254"
   "Microbicides"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1136254---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:26129676")
    ("publications_info"
     "{\"PMID:26129676\": {\"publication date\": \"2015 Aug\", \"sentence\": \"In this study, we report that one of the antimicrobial peptides scolopendrasin VII, derived from Scolopendra subspinipes mutilans, stimulates actin polymerization and the subsequent chemotactic migration of macrophages through the activation of ERK and protein kinase B (Akt) activity.\", \"subject score\": 764, \"object score\": 1000}}")
    ("subject" "UMLS:C1136254"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0051821"
   "andrographolide"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0051821---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:27475717")
    ("publications_info"
     "{\"PMID:27475717\": {\"publication date\": \"2016 09 15\", \"sentence\": \"In addition, ERK and GSK3beta-dependent C/EBPbeta phosphorylation was attenuated by andrographolide.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0051821"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0178601"
   "Dopamine Agonists"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0178601---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:26153447")
    ("publications_info"
     "{\"PMID:26153447\": {\"publication date\": \"2015 Oct\", \"sentence\": \"Pretreatment with a positive allosteric modulator (PAM) of muscarinic acetylcholine M4 receptors (M4Rs), VU0152100, attenuated the D1R agonist-stimulated ERK phosphorylation in the two regions, whereas the PAM itself did not alter basal ERK phosphorylation.\", \"subject score\": 819, \"object score\": 819}}")
    ("subject" "UMLS:C0178601"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0002932"
   "Anesthetics"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0002932---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:29186909")
    ("publications_info"
     "{\"PMID:29186909\": {\"publication date\": \"2017 Nov 27\", \"sentence\": \"Ketamine, a Clinically Used Anesthetic, Inhibits Vascular Smooth Muscle Cell Proliferation via PP2A-Activated PI3K/Akt/ERK Inhibition.\", \"subject score\": 1000, \"object score\": 741}}")
    ("subject" "UMLS:C0002932"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0962192"
   "Bay 11-7085"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0962192---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:26993765")
    ("publications_info"
     "{\"PMID:26993765\": {\"publication date\": \"2016 Apr 26\", \"sentence\": \"Inhibition of autophagy markedly decreased endogenous and BAY 11-7085-induced ERK phosphorylation, suggesting a positive feed back loop between ERK activation and autophagy in synovial fibroblasts.\", \"subject score\": 816, \"object score\": 816}}")
    ("subject" "UMLS:C0962192"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0243192"
   "agonists"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0243192---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:20553736|PMID:26993765|PMID:27551480")
    ("publications_info"
     "{\"PMID:27551480\": {\"publication date\": \"2015\", \"sentence\": \"Furthermore, levels of phosphor-TSC2 (Ser664) were increased after treatment with FVII and PAR2 agonist whereas these were significantly abolished in the presence of a potent and specific MEK/ERK inhibitor U0126.\", \"subject score\": 861, \"object score\": 729}, \"PMID:26993765\": {\"publication date\": \"2016 Apr 26\", \"sentence\": \"Both BAY 11-7085-induced autophagy and GR activation were down regulated with PPAR-gamma agonist, 15d-PGJ2 and MEK/ERK inhibitor UO126.\", \"subject score\": 888, \"object score\": 764}, \"PMID:20553736\": {\"publication date\": \"2010 Jul 03\", \"sentence\": \"The effect was synergistically augmented by PPARgamma agonist, but attenuated by inhibitors of PPARgamma, ERK or p38 MAPK.\", \"subject score\": 938, \"object score\": 1000}}")
    ("subject" "UMLS:C0243192"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0040616"
   "Anti-Anxiety Agents"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0040616---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:27876502|PMID:29137241")
    ("publications_info"
     "{\"PMID:29137241\": {\"publication date\": \"2017 Oct 10\", \"sentence\": \"CZ415 could be further tested as a promising anti-osteosarcoma agent, alone or in combination of ERK inhibition.\", \"subject score\": 844, \"object score\": 861}, \"PMID:27876502\": {\"publication date\": \"2017 Jan 04\", \"sentence\": \"CONCLUSION: The results indicate that OD as an anti-metastatic agent suppresses the metastatic response by targeting p-ERK, p-38 and NF-kappaB, thus reducing the invasion capacity of MCF-7 breast cancer cells through inhibition of MMP-9 and ICAM-1 expression and plays an important role in the regulation of breast cancer cell apoptosis.\", \"subject score\": 896, \"object score\": 790}}")
    ("subject" "UMLS:C0040616"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1373060"
   "Nitric Oxide Synthase Inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1373060---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:22160804")
    ("publications_info"
     "{\"PMID:22160804\": {\"publication date\": \"2012 Apr\", \"sentence\": \"The effects of iNOS inhibitors were associated with diminished ouabain tyrosine nitration as well as abrogation of ouabain-induced p38 and ERK phosphorylation.\", \"subject score\": 937, \"object score\": 861}}")
    ("subject" "UMLS:C1373060"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0003209"
   "Anti-Inflammatory Agents"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003209---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:21347512")
    ("publications_info"
     "{\"PMID:21347512\": {\"publication date\": \"2011 May\", \"sentence\": \"Aspirin, an anti-inflammatory drug, is a known ERK inhibitor and prevents neurodegenerative disorders including prion diseases.\", \"subject score\": 983, \"object score\": 785}}")
    ("subject" "UMLS:C0003209"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1171350"
   "kinase inhibitor [EPC]"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1171350---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:19127349|PMID:19890601")
    ("publications_info"
     "{\"PMID:19127349\": {\"publication date\": \"2009 Feb\", \"sentence\": \"The loss in countering capacity of leptin on the ethanol-induced cytotoxicity was attained with Src kinase inhibitor, PP2, and EGFR kinase inhibitor, AG1478, as well as ERK inhibitor, PD98059.\", \"subject score\": 901, \"object score\": 827}, \"PMID:19890601\": {\"publication date\": \"2010 Jul\", \"sentence\": \"The sole systemic therapy that has shown efficacy in improving the survival of HCC patients is sorafenib, an oral kinase inhibitor that blocks the Raf/MEK/ERK pathway and the receptor for VEGFR 2 and PDGFR-beta.\", \"subject score\": 901, \"object score\": 763}}")
    ("subject" "UMLS:C1171350"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0002932"
   "Anesthetics"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0002932---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:17492663")
    ("publications_info"
     "{\"PMID:17492663\": {\"publication date\": \"2007 Oct 01\", \"sentence\": \"Volatile anesthetics affect the morphology of rat glioma C6 cells via RhoA, ERK, and Akt activation.\", \"subject score\": 888, \"object score\": 1000}}")
    ("subject" "UMLS:C0002932"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C3543842"
   "Tonics"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C3543842---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:15327816")
    ("publications_info"
     "{\"PMID:15327816\": {\"publication date\": \"2004 Sep\", \"sentence\": \"The results from this study indicate that persistent ERK activation is required for the enhanced behavioral responses to spinal group I mGluR activation following inflammation and suggest that tonic modulation of ERK activity may underlie a component of central sensitization in dorsal horn neurons.\", \"subject score\": 661, \"object score\": 861}}")
    ("subject" "UMLS:C3543842"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1517132"
   "Farnesyl Transferase Inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1517132---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:16712893")
    ("publications_info"
     "{\"PMID:16712893\": {\"publication date\": \"2006 Sep 15\", \"sentence\": \"Both FTIs successfully inhibited the ERK and activated JNK in RIE/K-ras cells.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C1517132"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1579373"
   "Drugs used for the treatment of acute migraine"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1579373---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:16020508")
    ("publications_info"
     "{\"PMID:16020508\": {\"publication date\": \"2005 Nov 01\", \"sentence\": \"Thus, Tregs share biochemical characteristics of anergy, including abortive activation of Ras-MEK-Erk, increased activation of Rap1, and increased expression of p27kip1.\", \"subject score\": 694, \"object score\": 660}}")
    ("subject" "UMLS:C1579373"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0243042"
   "Inflammation Mediators"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0243042---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:15379598|PMID:22253793|PMID:30455267")
    ("publications_info"
     "{\"PMID:15379598\": {\"publication date\": \"2004 Sep\", \"sentence\": \"In particular, mitogen-activated protein kinase (MAPK), such as ERK and p38, is activated by inflammatory mediators in primary sensory and secondary order dorsal horn neurons and participates in the generation and maintenance of inflammatory pain.\", \"subject score\": 964, \"object score\": 1000}, \"PMID:22253793\": {\"publication date\": \"2012\", \"sentence\": \"Furthermore, TLR agonists or live pathogen (S. aureus, P. aeruginosa, & C. albicans)-challenged Muller glia produced significantly higher levels of inflammatory mediators (TNF-alpha, IL-1beta, IL-6 and IL-8), concomitantly with the activation of NF-kappaB, p38 and Erk signaling.\", \"subject score\": 964, \"object score\": 861}, \"PMID:30455267\": {\"publication date\": \"2018 Dec 03\", \"sentence\": \"Mechanistically, TLR8, localizing in the endosomes and lysosomes, mediated ERK activation, inflammatory mediators' production, and neuronal hyperexcitability after SNL.\", \"subject score\": 877, \"object score\": 627}}")
    ("subject" "UMLS:C0243042"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0304403"
   "Psychostimulant (substance)"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0304403---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:16954211")
    ("publications_info"
     "{\"PMID:16954211\": {\"publication date\": \"2006 Oct 27\", \"sentence\": \"In contrast, psychostimulants activate ERK and induce hyperactivity in normal animals.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0304403"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1519313"
   "Signal Transduction Inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1519313---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:15655409")
    ("publications_info"
     "{\"PMID:15655409\": {\"publication date\": \"2005 Feb\", \"sentence\": \"BAY 43-9006 (BAY) is a novel signal transduction inhibitor that prevents tumor cell proliferation and angiogenesis through blockade of the Raf/MEK/ERK pathway at the level of Raf kinase and the receptor tyrosine kinases vascular endothelial growth factor receptor-2 and platelet-derived growth factor receptor-beta.\", \"subject score\": 916, \"object score\": 763}}")
    ("subject" "UMLS:C1519313"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1514727"
   "Ras Inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1514727---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:12812842")
    ("publications_info"
     "{\"PMID:12812842\": {\"publication date\": \"2003 Jul 03\", \"sentence\": \"Ras inhibitor, Erk blocker or phosphatidylinositol 3-kinase inhibitor decreased depolarization- or NO donor-promoted survival.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C1514727"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1268567"
   "Protein-tyrosine kinase inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1268567---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:11978788|PMID:22773546|PMID:30258794")
    ("publications_info"
     "{\"PMID:11978788\": {\"publication date\": \"2002 Jun 28\", \"sentence\": \"In rat extensor digitorum longus (EDL) muscles, (a) AMPK activator, 5-aminoimidazole-4-carboxamide-1-beta-d-riboside (AICAR), activated PYK2, ERK and aPKCs; (b) effects of AICAR on ERK and aPKCs were blocked by tyrosine kinase inhibitor, genistein, and MEK1 inhibitor, PD98059; and (c) effects of AICAR on aPKCs and 2-deoxyglucose (2-DOG) uptake were inhibited by genistein, PD98059, and PLD-inhibitor, 1-butanol.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:30258794\": {\"publication date\": \"2018 Sep-Oct\", \"sentence\": \"The following groups of systemic drugs have been considered: taxanes, epidermal growth factor-receptor (EGFR) inhibitors, EGFR tyrosine kinase inhibitors, tyrosine kinase inhibitors, inhibitors of MEK/ERK, BRAF inhibitors, CD20 antagonists, vascular endothelial growth factor inhibitors, and retinoids.\", \"subject score\": 1000, \"object score\": 694}, \"PMID:22773546\": {\"publication date\": \"2013 Oct\", \"sentence\": \"In cells lines with complex activation profiles (HSC39 and OE33), a combination of TKIs or Mek inhibition (in nM concentrations) was necessary for cytotoxicity and inhibition of Erk and Akt phosphorylation.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C1268567"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1258619"
   "decursinol"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1258619---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:12677526")
    ("publications_info"
     "{\"PMID:12677526\": {\"publication date\": \"2003 Mar\", \"sentence\": \"G-Rd and DC attenuated, in part, the increased phospho-ERK and the decreased phospho-CREB protein levels.\", \"subject score\": 1000, \"object score\": 660}}")
    ("subject" "UMLS:C1258619"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1514555"
   "Protein Kinase C Inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1514555---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:12676927|PMID:12745093|PMID:20477948|PMID:20600853")
    ("publications_info"
     "{\"PMID:12676927\": {\"publication date\": \"2003 Jun 27\", \"sentence\": \"Inhibition of the ERK and protein kinase C signaling pathways with the MEK-1 inhibitor, U0126, and protein kinase C inhibitor, GF 1092030x, respectively, and chelating intracellular free calcium with 1,2-bis(2-aminophenoyl)ethane-N,N,N',N'-tetraacetic acid-AM, which also reduced ERK1/2 activation, significantly reduced H2O2-induced AA release in MC+/+ expressing either group IIa or V PLA2s.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:12745093\": {\"publication date\": \"2003 May 30\", \"sentence\": \"The PMA-induced MMP-9 secretion was abolished by treatment of a pan-protein kinase C (PKC) inhibitor, GF109203X, and an inhibitor of NF-kappaB activation, sulfasalazine, and partly inhibited by treatment of inhibitors of ERK pathway, PD98059 and U0126.\", \"subject score\": 937, \"object score\": 861}, \"PMID:20477948\": {\"publication date\": \"2010 Aug\", \"sentence\": \"In addition, protein kinase C inhibitors suppressed ATP-induced ERK and JNK activation, and also inhibited ATP-induced CXCL2 expression in microglia.\", \"subject score\": 983, \"object score\": 623}, \"PMID:20600853\": {\"publication date\": \"2010 Oct\", \"sentence\": \"The protein kinase C inhibitors, RO 320432 and GO 6983, and the ERK inhibitors UO 126 and PD 98059 all activated PDE4A4 aggregate formation, whilst roscovitine, thalidomide and the tyrosine kinase inhibitors, genistein and AG17, all inhibited this process.\", \"subject score\": 983, \"object score\": 824}}")
    ("subject" "UMLS:C1514555"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0003402"
   "Antioxidants"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003402---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:14647418")
    ("publications_info"
     "{\"PMID:14647418\": {\"publication date\": \"2004 Feb 19\", \"sentence\": \"Lastly, antioxidants (e.g., L-N-acetylcysteine; L-NAC) opposed adaphostin-mediated mitochondrial dysfunction, Raf-1/MEK/ERK downregulation, JNK activation, and apoptosis.\", \"subject score\": 1000, \"object score\": 794}}")
    ("subject" "UMLS:C0003402"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0597220"
   "phospholipase inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0597220---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:12759139")
    ("publications_info"
     "{\"PMID:12759139\": {\"publication date\": \"2003 Jun 13\", \"sentence\": \"Pertussis toxin, an inhibitor of G(i)/G(o) protein, and phospholipase C (PLC) inhibitor blocked Lkn-1-induced activation of ERK.\", \"subject score\": 890, \"object score\": 1000}}")
    ("subject" "UMLS:C0597220"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1513344"
   "Mitogen-Activated Protein Kinase Inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1513344---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:12403788|PMID:19878271|PMID:29777202|PMID:31807022")
    ("publications_info"
     "{\"PMID:12403788\": {\"publication date\": \"2003 Jan 10\", \"sentence\": \"SB203580, a p38 MAPK inhibitor, and PD98059, an ERK inhibitor, but not wortmannin a phosphatidylinositol 3-kinase (PI3K) inhibitor, prevented AA toxicity in pyrazole hepatocytes and E47 cells.\", \"subject score\": 938, \"object score\": 827}, \"PMID:19878271\": {\"publication date\": \"2010 Feb 01\", \"sentence\": \"Compared with PD98059, an MAPK inhibitor, baicalein exhibited a stronger inhibitory effect on Erk(1/2) phosphorylation.\", \"subject score\": 983, \"object score\": 1000}, \"PMID:29777202\": {\"publication date\": \"2018 May 18\", \"sentence\": \"In these resistant cells, phosphorylation of ribosomal protein S6 (rpS6) but not phosphorylation of ERK or p90 ribosomal S6 kinase (RSK) were unable to be inhibited by MAPK pathway inhibitors.\", \"subject score\": 901, \"object score\": 1000}, \"PMID:31807022\": {\"publication date\": \"2019\", \"sentence\": \"Finally, ERK was inhibited using a mitogen-activated protein kinase/ERK kinase inhibitor (U0126) to further explore the molecular mechanism of GOF-mutant SHP2 affecting GBM cells.\", \"subject score\": 927, \"object score\": 1000}}")
    ("subject" "UMLS:C1513344"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1443775"
   "Epidermal growth factor receptor inhibitor (product)"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1443775---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:11948693|PMID:12402303|PMID:21904878|PMID:28791489")
    ("publications_info"
     "{\"PMID:11948693\": {\"publication date\": \"2002\", \"sentence\": \"Proliferation, activation of ERK, and biosynthesis of Egr-1 was completely inhibited in EGF or thrombin-treated HaCaT cells by the MAP kinase kinase inhibitor PD98059 and by AG1487, an EGF receptor-specific tyrosine kinase inhibitor.\", \"subject score\": 932, \"object score\": 1000}, \"PMID:12402303\": {\"publication date\": \"2002 Dec 01\", \"sentence\": \"Tyrphostin AG1478, an EGFR tyrosine kinase inhibitor, blocks ALP-induced MAPK/ERK activation but not EGFR internalization.\", \"subject score\": 1000, \"object score\": 562}, \"PMID:21904878\": {\"publication date\": \"2011 Oct\", \"sentence\": \"Pretreatment of beta-cells with HNMPA, an insulin receptor inhibitor, and AG1478, an epidermal growth factor receptor inhibitor, further increased the cAMP level and Erk phosphorylation in the presence of exendin-4 (exe-4), a GLP-1 agonist.\", \"subject score\": 1000, \"object score\": 861}, \"PMID:28791489\": {\"publication date\": \"2017 Nov\", \"sentence\": \"Sustained activation of ERK by overexpression of constitutively active MEK1 was sufficient to expand CD44+/CD24- populations in cells in which EGFR activity was blocked by either erlotinib, an EGFR kinase inhibitor, or BB-94, a metalloprotease inhibitor that prevents generation of soluble EGFR ligands.\", \"subject score\": 938, \"object score\": 1000}}")
    ("subject" "UMLS:C1443775"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0597217"
   "phosphatase inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0597217---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:16464862")
    ("publications_info"
     "{\"PMID:16464862\": {\"publication date\": \"2006 Apr 21\", \"sentence\": \"In contrast, p38 MAPK inhibitors had no detectable effect on the ERK activation induced by fibroblast growth factor 2 or pervanadate, a phosphatase inhibitor, and MEK inhibitors did not influence p38 MAPK phosphorylation, confirming both the specificity and unidirectionality of p38 MAPK-ERK cross-talk.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0597217"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1513344"
   "Mitogen-Activated Protein Kinase Inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1513344---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:11466422|PMID:11684667|PMID:14701813|PMID:31776277")
    ("publications_info"
     "{\"PMID:11466422\": {\"publication date\": \"2001 Aug 01\", \"sentence\": \"PD98059, a selective MAPK kinase inhibitor, had a smaller and only marginally significant effect on CR acquisition, although it did block the learning-related increases in ERK activity in both the hippocampus and anterior vermis.\", \"subject score\": 911, \"object score\": 861}, \"PMID:14701813\": {\"publication date\": \"2004 Mar 12\", \"sentence\": \"JNK, p38, and ERK activation seem not to be required for this type of cell death because mitogen-activated protein kinase inhibitors did not significantly affect TNF-induced necrotic cell death.\", \"subject score\": 988, \"object score\": 1000}, \"PMID:11684667\": {\"publication date\": \"2001 Nov\", \"sentence\": \"We find that Erk MAP kinase is normally active in ureteric bud, and that inhibiting Erk activation with the MAP kinase kinase inhibitor, PD98059, reversibly inhibits branching in a dose-dependent manner, while allowing tubule elongation to continue.\", \"subject score\": 941, \"object score\": 627}, \"PMID:31776277\": {\"publication date\": \"2019 Nov 27\", \"sentence\": \"Using MAPK inhibitors, we found that LMP1 activates ERK or p38 to repress the expression of DUSP6 and DUSP8, with corresponding substrate specificity.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C1513344"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0085387"
   "Cyclooxygenase Inhibitors"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0085387---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:16458997")
    ("publications_info"
     "{\"PMID:16458997\": {\"publication date\": \"2006 Jul\", \"sentence\": \"Both the cyclooxygenase inhibitor, indomethacin and the ERK inhibitors, UO126 and PD980589 reverse the hypoxia-induced increase in intracellular cAMP levels back to those seen in normoxic hPASM cells.\", \"subject score\": 1000, \"object score\": 861}}")
    ("subject" "UMLS:C0085387"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0600437"
   "Nitric Oxide Donors"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0600437---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:10744637")
    ("publications_info"
     "{\"PMID:10744637\": {\"publication date\": \"2000 Apr\", \"sentence\": \"The STE-induced increase in phospho-ERK was suppressed by NO donors and the cGMP mimetic, and reversed by cGMP-PKG inhibitor, as was expression of AML1B and DNA binding in nuclear extracts.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0600437"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1443643"
   "Proteasome inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1443643---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:10617109")
    ("publications_info"
     "{\"PMID:10617109\": {\"publication date\": \"2000 Jan\", \"sentence\": \"Uniquely, the kinetics of MAP kinase activation induced by proteasome inhibitors are very slow compared with those resulting from activation by nerve growth factor; ERK activation is detectable only after a 5-h treatment with the inhibitors, and its activity remained unchanged for at least until 27 h.\", \"subject score\": 983, \"object score\": 1000}}")
    ("subject" "UMLS:C1443643"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1514555"
   "Protein Kinase C Inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1514555---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:10523856|PMID:15863357|PMID:22561846")
    ("publications_info"
     "{\"PMID:10523856\": {\"publication date\": \"1999 Oct 14\", \"sentence\": \"The MEK inhibitor PD 098059, as well as the PKC inhibitors, completely blocked TPA-mediated ERK activation.\", \"subject score\": 983, \"object score\": 547}, \"PMID:15863357\": {\"publication date\": \"2005 Apr 15\", \"sentence\": \"DMS had no effect on the dbcAMP-induced membrane translocation of protein kinase C (PKC) isozymes, and PKC inhibitors had no significant effect on ERK activation.\", \"subject score\": 983, \"object score\": 1000}, \"PMID:22561846\": {\"publication date\": \"2012 Aug\", \"sentence\": \"The protein kinase C inhibitor, Ro-31-7459, is a potent activator of ERK and JNK MAP kinases in HUVECs and yet inhibits cyclic AMP-stimulated SOCS-3 gene induction through inactivation of the transcription factor c-Jun.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C1514555"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1268567"
   "Protein-tyrosine kinase inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1268567---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:9933031")
    ("publications_info"
     "{\"PMID:9933031\": {\"publication date\": \"1999 Feb 15\", \"sentence\": \"On the other hand, an obligatory tyrosine phosphorylation step for activation of ERK was indicated by the use of protein tyrosine kinase inhibitors, which profoundly inhibited the activation of ERK by EGF, Ang II, and PMA.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C1268567"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0162754"
   "Serotonin 5-HT1 Receptor Agonists"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0162754---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:9658404")
    ("publications_info"
     "{\"PMID:9658404\": {\"publication date\": \"1998 Jul\", \"sentence\": \"We further demonstrate that 5-HT1 agonists inactivate ERK by dephosphorylation, even in the presence of constitutively activated MEK1.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0162754"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1514727"
   "Ras Inhibitor"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1514727---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:16052566|PMID:8969227")
    ("publications_info"
     "{\"PMID:8969227\": {\"publication date\": \"1996 Dec 27\", \"sentence\": \"Although pretreatment with manumycin, a Ras farnesyltransferase inhibitor, or overexpression of a dominant-negative mutant of Ras inhibited insulin-induced ERK activation, neither affected AngII-induced activation of ERKs.\", \"subject score\": 901, \"object score\": 583}, \"PMID:16052566\": {\"publication date\": \"2005 Sep 15\", \"sentence\": \"Our data also indicated that ERK activation and the potentiation of ATP calcium responses were sensitive to the src-like kinase inhibitor herbimycin A, p21(ras) farnesyltransferase inhibitor peptide, and some PKC inhibitors.\", \"subject score\": 861, \"object score\": 1000}}")
    ("subject" "UMLS:C1514727"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1171350"
   "kinase inhibitor [EPC]"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1171350---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:20149136|PMID:28197555")
    ("publications_info"
     "{\"PMID:28197555\": {\"publication date\": \"2017 Jan-Feb\", \"sentence\": \"This activation was blocked by a MAPK kinase inhibitor, suggesting that similar pathways are involved in activation of ERK and p38 MAPK.\", \"subject score\": 901, \"object score\": 1000}, \"PMID:20149136\": {\"publication date\": \"2010 Apr\", \"sentence\": \"PLX4032, a selective BRAF(V600E) kinase inhibitor, activates the ERK pathway and enhances cell migration and proliferation of BRAF melanoma cells.\", \"subject score\": 1000, \"object score\": 861}}")
    ("subject" "UMLS:C1171350"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0040615"
   "Antipsychotic Agents"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0040615---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:24203573")
    ("publications_info"
     "{\"PMID:24203573\": {\"publication date\": \"2014 Mar\", \"sentence\": \"Taken together, our data suggest that (+/-)-alpha-lipoic acid exerts synergistic effects with haloperidol on the Akt/GSK-3beta pathway, potentially involved in the therapeutic effects of APs, and antagonism of ERK activation and D2R upregulation, potentially involved in tardive dyskinesia and treatment resistance.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0040615"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0003289"
   "Antidepressants"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003289---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:25476565|PMID:30692946")
    ("publications_info"
     "{\"PMID:25476565\": {\"publication date\": \"2015 Mar 01\", \"sentence\": \"Results demonstrate that alarin may exert antidepressant-like effects by targeting TrkB receptor-mediated ERK and AKT signal systems, which could help to identify the alarin receptor.\", \"subject score\": 1000, \"object score\": 623}, \"PMID:30692946\": {\"publication date\": \"2018\", \"sentence\": \"Blockade of the ERK-CREB axis with the ERK-specific inhibitor U0126 repressed the neuroprotective and antidepressant-like effects of paeoniflorin on rats in the setting of chronic-mild-stress and abolished the recoveries of p-ERK mediated by paeoniflorin treatment.\", \"subject score\": 1000, \"object score\": 861}}")
    ("subject" "UMLS:C0003289"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0003289"
   "Antidepressants"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003289---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:25476565|PMID:27634096|PMID:31768875")
    ("publications_info"
     "{\"PMID:25476565\": {\"publication date\": \"2015 Mar 01\", \"sentence\": \"This resulted in an absence of antidepressant-like effects, as well as no activation of ERK and AKT signaling pathways.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:27634096\": {\"publication date\": \"2016 12\", \"sentence\": \"Together, the results demonstrate that these three different rapid acting antidepressant agents increase ERK signaling and BDNF release in an activity dependent manner that leads to increased neuronal complexity.\", \"subject score\": 748, \"object score\": 861}, \"PMID:31768875\": {\"publication date\": \"2019 Nov 25\", \"sentence\": \"The antidepressant-like effect of guanosine is dependent on GSK-3beta inhibition and activation of MAPK/ERK and Nrf2/heme oxygenase-1 signaling pathways.\", \"subject score\": 1000, \"object score\": 694}}")
    ("subject" "UMLS:C0003289"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0013036"
   "Dopaminergic Agents"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0013036---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:24380761")
    ("publications_info"
     "{\"PMID:24380761\": {\"publication date\": \"2014 Mar\", \"sentence\": \"The selective increase in ERK activity in the PFC associated with behavioral sensitization, points to a possible pivotal role of the dopamine projection to the medial frontal cortex in the mediation of neural plasticity, considered to underlie the sensitization processes induced by dopaminergic drugs.\", \"subject score\": 1000, \"object score\": 861}}")
    ("subject" "UMLS:C0013036"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0013227"
   "Pharmaceutical Preparations"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0013227---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications"
     "PMID:11170175|PMID:12151388|PMID:12163544|PMID:14697665|PMID:19683494|PMID:20668238|PMID:21645515|PMID:24348046|PMID:27614430|PMID:29291014|PMID:31649535")
    ("publications_info"
     "{\"PMID:24348046\": {\"publication date\": \"2013\", \"sentence\": \"Additionally, such a combination is expected to reduce MEKi-induced skin toxicities, as these drugs are thought to have antagonistic effects on ERK activation in keratinocytes.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:11170175\": {\"publication date\": \"2001 Feb 01\", \"sentence\": \"Doxorubicin (0.2 microM), an anticancer drug whose mechanism of action is independent of microtubules, also induced ERK activation, tau phosphorylation and apoptosis.\", \"subject score\": 861, \"object score\": 645}, \"PMID:12151388\": {\"publication date\": \"2002 Oct 04\", \"sentence\": \"Hence, anti-calmodulin drugs (such as W13, trifluoroperazine, or W7) are able to induce Ras/ERK pathway activation under low levels of growth factors.\", \"subject score\": 790, \"object score\": 645}, \"PMID:12163544\": {\"publication date\": \"2002 Aug\", \"sentence\": \"These drugs also inhibit TGFbeta1 effects on Akt/PKB phosphorylation but have no effect on TGFbeta1-evoked Erk activation.\", \"subject score\": 1000, \"object score\": 583}, \"PMID:14697665\": {\"publication date\": \"2003 Dec\", \"sentence\": \"Furthermore, glial cell elimination or inactivation in the culture, by gliotoxic drugs, abrogates NO-induced ERK activation.\", \"subject score\": 861, \"object score\": 619}, \"PMID:19683494\": {\"publication date\": \"2009 Aug 14\", \"sentence\": \"Upregulation of Mig6 by lipid agonists such as LPA and S1P or actin drugs involved MAL and correlated with decreased activation of EGFR, MAPK/Erk, and c-fos.\", \"subject score\": 888, \"object score\": 694}, \"PMID:21645515\": {\"publication date\": \"2011 Aug 15\", \"sentence\": \"Nevertheless, RalA was not only cytoprotective against multiple chemotherapeutic drugs, but also promigratory inducing stress fiber formation, which was accompanied by the activation of Akt and Erk pathways.\", \"subject score\": 828, \"object score\": 1000}, \"PMID:29291014\": {\"publication date\": \"2017 Dec 05\", \"sentence\": \"The pattern of upregulated proteins was similar with both drugs and indicates an activated RAF/MEK/ERK pathway, but more proteins were downregulated with sorafenib versus regorafenib.\", \"subject score\": 1000, \"object score\": 742}, \"PMID:27614430\": {\"publication date\": \"2016 Dec\", \"sentence\": \"Therefore, vanadium compounds can be regarded as a novel type of anticancer drugs through the prolonged activation of MAPK/ERK pathway but retained AKT activity.\", \"subject score\": 861, \"object score\": 802}, \"PMID:20668238\": {\"publication date\": \"2010 Aug 17\", \"sentence\": \"In contrast, the drug activates MEK and ERK phosphorylation in cells with wild-type BRAF.\", \"subject score\": 1000, \"object score\": 861}, \"PMID:31649535\": {\"publication date\": \"2019\", \"sentence\": \"Activations of Akt or ERK pathway induced by clinical drugs promote therapeutic failure due to decrease of drug response, and no available strategies have been developed to solve these problems.\", \"subject score\": 888, \"object score\": 861}}")
    ("subject" "UMLS:C0013227"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0243192"
   "agonists"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0243192---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications"
     "PMID:10187821|PMID:10537288|PMID:10760082|PMID:15149321|PMID:15153488|PMID:15632168|PMID:15944153|PMID:16278303|PMID:16709153|PMID:17312106|PMID:18261471|PMID:19347570|PMID:19501623|PMID:19662020|PMID:21541969|PMID:22306083|PMID:22796106|PMID:23750009|PMID:23991110|PMID:24140231|PMID:24151242|PMID:26107116|PMID:27349565|PMID:27456816|PMID:31266805|PMID:9687510|PMID:9870938|PMID:9933031|PMID:9990315")
    ("publications_info"
     "{\"PMID:24151242\": {\"publication date\": \"2013 Dec 01\", \"sentence\": \"However, this phenotype is most obvious on a G(389)-beta1AR background; the more robust agonist-dependent cAMP/PKA and ERK responses in R(389)-beta1AR cells effectively obscure the effect of the S49G polymorphism.\", \"subject score\": 740, \"object score\": 861}, \"PMID:23991110\": {\"publication date\": \"2013\", \"sentence\": \"Agonist-induced Erk phosphorylation was preceded by rapid FGFR and EGFR transactivation; however, only EGFR inhibition blocked Erk activation and proliferation.\", \"subject score\": 775, \"object score\": 775}, \"PMID:23750009\": {\"publication date\": \"2013 Aug 15\", \"sentence\": \"The effects of ARRBs were isoform specific; ARRB2 inhibited MC1R agonist-dependent cAMP production but not ERK activation, stimulated internalization and showed prolonged co-localization with the receptor in endocytic vesicles.\", \"subject score\": 799, \"object score\": 660}, \"PMID:24140231\": {\"publication date\": \"2013 Nov 15\", \"sentence\": \"Agonist-induced thromboxane A2 (TxA2) generation and ERK phosphorylation were significantly inhibited by TBCA.\", \"subject score\": 833, \"object score\": 861}, \"PMID:9687510\": {\"publication date\": \"1998 Aug 03\", \"sentence\": \"In HeLa, PC12 and SK-N-MC cells, PD 98059 and SB 203580 are both required to suppress the activation of MSK1 by TNF, NGF and FGF, respectively, because these agonists activate both the MAPK/ERK and SAPK2/p38 cascades.\", \"subject score\": 1000, \"object score\": 694}, \"PMID:9990315\": {\"publication date\": \"1998 Dec 15\", \"sentence\": \"TPO by itself did not activate ERK1, ERK2 and protein kinase C (PKC), whereas TPO directly enhanced the PKC-dependent activation of ERKs induced by other agonists including thrombin and phorbol esters, without affecting the PKC activation by those agonists.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:9933031\": {\"publication date\": \"1999 Feb 15\", \"sentence\": \"Our results show that although activation of PKC was critical for mitogenesis induced by Ang II or EGF, the initial activation of ERK by both agonists in these cells was essentially independent of PKC activation and was insensitive to Ca2+ mobilization.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:9870938\": {\"publication date\": \"1999 Jan 01\", \"sentence\": \"Parallel immunofluorescence confocal microscopy, using an anti-ERK antibody, showed that agonist-induced time-dependent ERK IR trafficking into perinuclear and nuclear loci was impaired in the internalization-defective cells.\", \"subject score\": 763, \"object score\": 763}, \"PMID:10187821\": {\"publication date\": \"1999 Apr 09\", \"sentence\": \"Point mutations within highly conserved regions of TM6 and intracellular loop 3 were without effect on agonist-stimulated ERK activation.\", \"subject score\": 645, \"object score\": 645}, \"PMID:10537288\": {\"publication date\": \"1999 Oct 15\", \"sentence\": \"Elimination of extracellular Ca2+ by EGTA also did not abolish the activation of ERK by GnRHa.\", \"subject score\": 916, \"object score\": 1000}, \"PMID:15944153\": {\"publication date\": \"2005 Jul 29\", \"sentence\": \"The mu agonist, [D-ala2,mephe4,glyol5]enkephalin (DAMGO), induces a transient stimulation of ERK phosphorylation, whereas kappa agonist, U69,593, engenders sustained ERK activation.\", \"subject score\": 888, \"object score\": 660}, \"PMID:18261471\": {\"publication date\": \"2008 Mar\", \"sentence\": \"This calcilytic also attenuated CaR agonist-induced ERK activation in these cells.\", \"subject score\": 520, \"object score\": 520}, \"PMID:17312106\": {\"publication date\": \"2007 Mar 01\", \"sentence\": \"BBPs did not inhibit activation of ERK induced by P2C, a TLR2/6 agonist.\", \"subject score\": 802, \"object score\": 1000}, \"PMID:15153488\": {\"publication date\": \"2004 Jun 01\", \"sentence\": \"ERK is also activated by CCR2 agonists, e.g., monocyte chemoattractant protein-1 (CCL2).\", \"subject score\": 861, \"object score\": 1000}, \"PMID:15632168\": {\"publication date\": \"2005 Mar 04\", \"sentence\": \"These results show that specific kinetics of ERK activation by agonists and dual efficacy ligands are determined, at least in part, by the differential ability of the two types of drugs to trigger mechanisms regulating deltaOR responsiveness.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:16709153\": {\"publication date\": \"2006 Sep 01\", \"sentence\": \"More fundamentally, these observations shed new light on enigmatic issues such as the inefficacy of S17N-Ras in blocking PE action or the role of the EGFR in heterologous agonist activation of the Ras/ERK pathway.\", \"subject score\": 623, \"object score\": 827}, \"PMID:22306083\": {\"publication date\": \"2012 May 15\", \"sentence\": \"We further investigated the apoptotic mechanism demonstrating that E2, as well as ESR1 and GPER specific agonists, induced sustained ERK, c-Jun and p38 phosphorylation, Cytochrome c release, caspase 3 and endogenous substrate Poly (ADP-ribose) polymerase (PARP) activation and increased expression of cell cycle inhibitor p21.\", \"subject score\": 827, \"object score\": 694}, \"PMID:22796106\": {\"publication date\": \"2012 Nov\", \"sentence\": \"Blockade of glutamate release achieved by the mGluR2/3 agonist, LY354740 or the selective adenosine A1R agonist, CCPA as well as neurotoxic lesions of lateral entorhinal cortex reduced the ability of SKF81297 to induce ERK activation in the dentate gyrus.\", \"subject score\": 802, \"object score\": 1000}, \"PMID:19662020\": {\"publication date\": \"2009 Oct\", \"sentence\": \"Furthermore, the inverse agonist activity olmesartan exerts against stretch-induced ERK activation requires an additional drug-receptor interaction involving the tetrazole group of olmesartan and Gln(257) of the AT(1) receptor.\", \"subject score\": 833, \"object score\": 566}, \"PMID:19501623\": {\"publication date\": \"2009 Dec\", \"sentence\": \"Thus, in intact brain HB-EGF, known to be expressed in brain, may be the major EGF agonist released in response to stimulation of alpha(2)-adrenoceptors, the released agonist(s) activate(s) EGF receptors, and ERK(1/2) is phosphorylated as a conventional response to EGF receptor activation.\", \"subject score\": 888, \"object score\": 1000}, \"PMID:27456816\": {\"publication date\": \"2016 07 26\", \"sentence\": \"We also demonstrate the ability of these new agonists to induce receptor internalization, ERK activation, and chemotaxis, all hallmarks of CXCR4 activation.\", \"subject score\": 888, \"object score\": 1000}, \"PMID:26107116\": {\"publication date\": \"2015 Oct\", \"sentence\": \"The TRHR1 agonist, taltirelin, injection in the BLA increased the level of p-ERK, which mimicked the increased p-ERK level in the BLA that was induced by treatment with repeated stress.\", \"subject score\": 861, \"object score\": 861}, \"PMID:27349565\": {\"publication date\": \"2016 Oct 10\", \"sentence\": \"To our knowledge this is the first report of ERK and JNK activation in MCF10A and MCF12A cells with P4, P4 metabolites, MPA, and MPR-Ag.\", \"subject score\": 861, \"object score\": 1000}, \"PMID:19347570\": {\"publication date\": \"2009 May\", \"sentence\": \"PPAR gamma partial agonist, KR-62776, inhibits adipocyte differentiation via activation of ERK.\", \"subject score\": 851, \"object score\": 1000}, \"PMID:16278303\": {\"publication date\": \"2006 Mar 01\", \"sentence\": \"ANXA1 and Ac2-26 acted as genuine agonists; Ac2-26 binding led to ERK activation in both FPR- and FPRL-1/ALX-transfected cells, while ANXA1 caused ERK activation only in cells transfected with FPRL-1/ALX.\", \"subject score\": 853, \"object score\": 1000}, \"PMID:15149321\": {\"publication date\": \"2004 Jun\", \"sentence\": \"The role of ERK and phosphorylation in PPAR gamma activation were studied, as were the effects of PPAR agonists on ERK activation and cell proliferation.\", \"subject score\": 888, \"object score\": 1000}, \"PMID:21541969\": {\"publication date\": \"2011 Jun 15\", \"sentence\": \"GnRH receptor levels correlated with induction of inositol phosphates, elevation of intracellular Ca(2+) , cytoskeletal actin reorganization, modulation of ERK activation and cell growth-inhibition with GnRH agonists.\", \"subject score\": 888, \"object score\": 1000}, \"PMID:10760082\": {\"publication date\": \"2000 Apr\", \"sentence\": \"In contrast to ADPKD, proliferation and ERK activity of cells derived from normal HKC were not stimulated by cAMP agonists, although electrogenic Cl- secretion was increased by these agonists in both ADPKD and HKC cell monolayers.\", \"subject score\": 802, \"object score\": 861}, \"PMID:31266805\": {\"publication date\": \"2019 Aug 16\", \"sentence\": \"We conclude that PTPN7 regulates platelet functional responses downstream of GPCR agonists, but not GPVI agonists, through inhibition of ERK activation and thromboxane generation.\", \"subject score\": 928, \"object score\": 1000}}")
    ("subject" "UMLS:C0243192"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C1443775"
   "Epidermal growth factor receptor inhibitor (product)"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1443775---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications"
     "PMID:16211241|PMID:18636178|PMID:19127349|PMID:19154417|PMID:19854188|PMID:22033246|PMID:24021351")
    ("publications_info"
     "{\"PMID:24021351\": {\"publication date\": \"2013 Nov 15\", \"sentence\": \"The EGFR inhibitor also inhibited the B[a]PDE-induced MEK/ERK and Akt signaling pathways and subsequently, suppressed COX-2 expression and promoter activity, in addition to suppressing the transactivation of AP-1 and NF-kappaB.\", \"subject score\": 1000, \"object score\": 549}, \"PMID:16211241\": {\"publication date\": \"2005 Nov\", \"sentence\": \"Paclitaxel-induced ERK and AKT activity was inhibited by the EGFR inhibitor, PD153035; ERK inhibitor, U0126; and PI3 kinase inhibitor, LY294002, respectively.\", \"subject score\": 1000, \"object score\": 827}, \"PMID:18636178\": {\"publication date\": \"2008 Aug\", \"sentence\": \"PD153035, an EGFR inhibitor, and U0126, an ERK inhibitor, inhibit atRA-induced upregulation of AQP3.\", \"subject score\": 1000, \"object score\": 827}, \"PMID:19154417\": {\"publication date\": \"2009 Mar\", \"sentence\": \"In these three high-sensitivity cells, the ERK pathway was activated without ligand stimulation, which was inhibited by EGFR TKI.\", \"subject score\": 1000, \"object score\": 861}, \"PMID:19127349\": {\"publication date\": \"2009 Feb\", \"sentence\": \"The loss in countering capacity of leptin on the ethanol-induced cytotoxicity was attained with Src kinase inhibitor, PP2, and EGFR kinase inhibitor, AG1478, as well as ERK inhibitor, PD98059.\", \"subject score\": 938, \"object score\": 827}, \"PMID:22033246\": {\"publication date\": \"2012 Jan 28\", \"sentence\": \"EGF induced the phosphorylation of EGFR, smad3, ERK, and JNK, and MMP-9 expression was decreased by the EGFR inhibitor, AG1478.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:19854188\": {\"publication date\": \"2009 Nov 19\", \"sentence\": \"Signal analyses showed that evodiamine stimulated the phosphorylation of EGFR, PKCalpha, and ERK, all of which were reduced by an EGFR inhibitor.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C1443775"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0243042"
   "Inflammation Mediators"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0243042---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:25501336")
    ("publications_info"
     "{\"PMID:25501336\": {\"publication date\": \"2014 Dec 09\", \"sentence\": \"These results suggest that pheophytin a functions by down-regulating the transcriptional levels of inflammatory mediators and blocking the ERK and STAT-1 pathways.\", \"subject score\": 964, \"object score\": 1000}}")
    ("subject" "UMLS:C0243042"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0003211"
   "Anti-Inflammatory Agents, Non-Steroidal"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003211---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:25406016")
    ("publications_info"
     "{\"PMID:25406016\": {\"publication date\": \"2015 Feb\", \"sentence\": \"In growth plate chondrocytes, inhibition of proliferation and ERK activation by NSAIDs is reversed by forskolin, 8-bromoadenosine, 3',5'-cAMP and a prostacyclin analog, iloprost.\", \"subject score\": 988, \"object score\": 1000}}")
    ("subject" "UMLS:C0003211"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0003402"
   "Antioxidants"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003402---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications"
     "PMID:12682429|PMID:15698432|PMID:21344382|PMID:24534200|PMID:25485998|PMID:27916558")
    ("publications_info"
     "{\"PMID:24534200\": {\"publication date\": \"2014 Sep 01\", \"sentence\": \"Moreover, ERK activation induced by PFHxS was blocked by MK801 but not antioxidants.\", \"subject score\": 888, \"object score\": 1000}, \"PMID:25485998\": {\"publication date\": \"2015 Apr\", \"sentence\": \"Several studies have suggested that ERK activation in lung cells has a protective effect in response to hyperoxia, through stimulation of DNA repair and antioxidant mechanisms, and prolonged cell survival.\", \"subject score\": 888, \"object score\": 1000}, \"PMID:27916558\": {\"publication date\": \"2017 Feb 05\", \"sentence\": \"Therefore, the cardioprotective effects of CAR may be attributed to its antioxidant and antiapoptotic activities through activations of the MAPK/ERK and Akt/eNOS signaling pathways.\", \"subject score\": 1000, \"object score\": 694}, \"PMID:12682429\": {\"publication date\": \"2003 Feb\", \"sentence\": \"To determine whether reactive oxygen species (ROS) play a role in mediating ERK activation and 6-OHDA toxicity, we examined the effects of catalase, superoxide dismutase (SOD1), and metalloporphyrin antioxidants ('SOD mimetics') on 6-OHDA-treated cells.\", \"subject score\": 888, \"object score\": 1000}, \"PMID:21344382\": {\"publication date\": \"2011 Dec\", \"sentence\": \"Our experimental results clearly demonstrated that induction of p-ERK and cell proliferation by arsenite is mediated via oxidative stress, since antioxidants can inhibit arsenite-induced cell transformation.\", \"subject score\": 1000, \"object score\": 861}, \"PMID:15698432\": {\"publication date\": \"2005 Mar\", \"sentence\": \"Activation of ERK in renal fibrosis after unilateral ureteral obstruction: modulation by antioxidants.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0003402"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0013162"
   "Drug Combinations"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0013162---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:23501104|PMID:25704960|PMID:28649441")
    ("publications_info"
     "{\"PMID:23501104\": {\"publication date\": \"2013 Apr 19\", \"sentence\": \"The enhanced therapeutic efficacy of the drug combination was achieved by a relatively transient blockade of the ERK pathway.\", \"subject score\": 966, \"object score\": 861}, \"PMID:28649441\": {\"publication date\": \"2017\", \"sentence\": \"Prospective simulations were then used to evaluate potential drug combinations and predictive biomarkers for increasing responsiveness to MEK/ERK inhibitors in these patients.\", \"subject score\": 901, \"object score\": 802}, \"PMID:25704960\": {\"publication date\": \"2015 Sep\", \"sentence\": \"The drug combination inactivated ERK, AKT, p70 S6K, and mTOR and activated JNK.\", \"subject score\": 966, \"object score\": 1000}}")
    ("subject" "UMLS:C0013162"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0026249"
   "Mitogens"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0026249---SEMMEDDB:stimulates---None---None---increased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:18334532|PMID:20030620|PMID:24212659")
    ("publications_info"
     "{\"PMID:24212659\": {\"publication date\": \"2011 Mar 08\", \"sentence\": \"They also completely block the activity of mitogens such as epidermal growth factor's ability to stimulate ERK and Ras.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:18334532\": {\"publication date\": \"2008 May\", \"sentence\": \"In vivo, recruitment of ERK and MSK is stimulated by mitogens, correlates with histone H3 phosphorylation and is impaired by Elk-1 knockdown.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:20030620\": {\"publication date\": \"2010\", \"sentence\": \"They also completely block the activity of mitogens such as epidermal growth factor's ability to stimulate ERK.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0026249"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0013227"
   "Pharmaceutical Preparations"
   "biolink:regulates"
   "NCBIGene:2048"
   "EPHB2"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0013227---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:2048---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:2048")
    ("predicate" "biolink:regulates")
    ("publications"
     "PMID:11408604|PMID:12543078|PMID:15843040|PMID:18223169|PMID:21251612|PMID:21879255|PMID:24559688|PMID:30482227")
    ("publications_info"
     "{\"PMID:24559688\": {\"publication date\": \"2014 May\", \"sentence\": \"The sensitivity to the drug was accompanied by a potent inhibition of both phospho-ERK and phospho-AKT, and a significant induction of apoptosis while absent in lines with intrinsic or acquired resistance.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:11408604\": {\"publication date\": \"2001 Jul\", \"sentence\": \"The Src-family tyrosine kinase inhibitor PP2 blocked MAPK activation by dopamine; however, this drug was also found to inhibit PDGF-BB-stimulated ERK activity and autophosphorylation of the PDGF receptor-beta.\", \"subject score\": 1000, \"object score\": 872}, \"PMID:12543078\": {\"publication date\": \"2002 Nov 24\", \"sentence\": \"Interestingly, all of the effective drugs inhibited the ERK activity, while the drugs had no effects on p38 MAPK activity and IkappaB degradation.\", \"subject score\": 888, \"object score\": 861}, \"PMID:18223169\": {\"publication date\": \"2008 Apr 01\", \"sentence\": \"Finally, CD38 stimulation of T2 B lymphocytes obtained from Btk-, Lyn-, or Fyn-deficient mice showed a defective differentiation; similarly, drugs interfering with PI3K or ERK decreased the proliferation or differentiation of this subset.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:15843040\": {\"publication date\": \"2005 Apr 15\", \"sentence\": \"While ERK1/2 activation was a general phenomenon, irrespective of the used cell type or antitumour drug, the MEK/ERK inhibitors only reduced cisplatin toxicity in human myeloid cells (THP-1, HL-60 and NB-4), but not in RAW 264.7 mouse macrophages and NRK-52E rat renal tubular cells; and failed to reduce the toxicity etoposide, camptothecin, melphalan and arsenic trioxide, in U-937 cells.\", \"subject score\": 861, \"object score\": 753}, \"PMID:21879255\": {\"publication date\": \"2012 Jan\", \"sentence\": \"Neither (ZEGFR:1907)2 nor any of the other             drugs were able to completely inactivate Akt or Erk.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:21251612\": {\"publication date\": \"2011 Jan 18\", \"sentence\": \"These drugs thus selectively inhibit ERK signaling in tumors with BRAF mutation.\", \"subject score\": 861, \"object score\": 861}, \"PMID:30482227\": {\"publication date\": \"2018 Nov 27\", \"sentence\": \"BACKGROUND: Drugs that inhibit the MEK/ERK pathway have therapeutic benefit in bladder cancer treatment but responses vary with patients, for reasons that are still not very clear.\", \"subject score\": 1000, \"object score\": 802}}")
    ("subject" "UMLS:C0013227"))
   (("id"
     "NCBIGene:2048---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:2048")))
  ("UMLS:C0017302"
   "General anesthetic drugs"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0017302---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:26400403")
    ("publications_info"
     "{\"PMID:26400403\": {\"publication date\": \"2015 Dec\", \"sentence\": \"While all three general anesthetics effectively blocked nociceptive responses and activation of ERK in the rat ACC following formalin injection during anesthesia, only sevoflurane inhibited ERK activation in the spinal cord and ACC at 24 h post-injection.\", \"subject score\": 901, \"object score\": 1000}}")
    ("subject" "UMLS:C0017302"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0304497"
   "Cytotoxic agent (product)"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0304497---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:29507530")
    ("publications_info"
     "{\"PMID:29507530\": {\"publication date\": \"2018\", \"sentence\": \"Conclusion: Taken together, our data identified nilotinib as a cytotoxic drug that combined with ERK inhibitors deserves to be tested as a novel therapy for adrenocortical carcinoma.\", \"subject score\": 1000, \"object score\": 861}}")
    ("subject" "UMLS:C0304497"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0597220"
   "phospholipase inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0597220---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:19048108")
    ("publications_info"
     "{\"PMID:19048108\": {\"publication date\": \"2008\", \"sentence\": \"A phospholipase Cgamma (PLCgamma) inhibitor and shRNA, as well as an Erk inhibitor, reduced ST6Gal1 and FUT9 mRNA levels and inhibited effects of L1 on neurite outgrowth and cell survival.\", \"subject score\": 913, \"object score\": 827}}")
    ("subject" "UMLS:C0597220"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0040616"
   "Anti-Anxiety Agents"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0040616---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30102992")
    ("publications_info"
     "{\"PMID:30102992\": {\"publication date\": \"2018 Nov 15\", \"sentence\": \"CONCLUSION: This study demonstrated that aqueous extracts from S. japonicus viscera are effective whitening and anti-aging agents that stimulate ERK signaling to inhibit melanin synthesis and promote collagen synthesis.\", \"subject score\": 901, \"object score\": 861}}")
    ("subject" "UMLS:C0040616"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0003211"
   "Anti-Inflammatory Agents, Non-Steroidal"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003211---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:16230075")
    ("publications_info"
     "{\"PMID:16230075\": {\"publication date\": \"2005 Oct\", \"sentence\": \"Exposure to NSAIDs inhibits Sp1 and ERK phosphorylation.\", \"subject score\": 1000, \"object score\": 861}}")
    ("subject" "UMLS:C0003211"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1514713"
   "Raf Kinase Inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1514713---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:18163502")
    ("publications_info"
     "{\"PMID:18163502\": {\"publication date\": \"2008 Jan\", \"sentence\": \"Components of the ERK pathway were inhibited by Raf-1 kinase inhibitor and the MEK inhibitor, PD98059.\", \"subject score\": 923, \"object score\": 861}}")
    ("subject" "UMLS:C1514713"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0003385"
   "Muscarinic Antagonists"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003385---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:19533734")
    ("publications_info"
     "{\"PMID:19533734\": {\"publication date\": \"2009 Dec\", \"sentence\": \"These effects of fustin were reversed by treatment with dicyclomine, a muscarinic M1 receptor antagonist, and SL327, a selective ERK inhibitor, but not by chelerythrine, a pan-protein kinase C (PKC) inhibitor.\", \"subject score\": 880, \"object score\": 793}}")
    ("subject" "UMLS:C0003385"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0729502"
   "Chemotherapeutic agent"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0729502---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30288546")
    ("publications_info"
     "{\"PMID:30288546\": {\"publication date\": \"2018 Dec\", \"sentence\": \"Combining chemotherapeutic agents with HDAC inhibitor (TSA) or with targeting Raf/MEK/ERK pathway is promising to circumvent chemoresistance in UCs.\", \"subject score\": 983, \"object score\": 754}}")
    ("subject" "UMLS:C0729502"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0598695"
   "NMDA receptor antagonist"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0598695---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:19361551")
    ("publications_info"
     "{\"PMID:19361551\": {\"publication date\": \"2009 Aug\", \"sentence\": \"ERK activation could indicate a stress-mediated increase in glutamatergic signaling, therefore mice were treated prior to SNI and stress with memantine, an N-methyl-D-aspartate receptor (NMDAR) antagonist.\", \"subject score\": 954, \"object score\": 1000}}")
    ("subject" "UMLS:C0598695"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1136254"
   "Microbicides"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1136254---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:26129676")
    ("publications_info"
     "{\"PMID:26129676\": {\"publication date\": \"2015 Aug\", \"sentence\": \"In this study, we report that one of the antimicrobial peptides scolopendrasin VII, derived from Scolopendra subspinipes mutilans, stimulates actin polymerization and the subsequent chemotactic migration of macrophages through the activation of ERK and protein kinase B (Akt) activity.\", \"subject score\": 764, \"object score\": 1000}}")
    ("subject" "UMLS:C1136254"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0051821"
   "andrographolide"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0051821---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:27475717")
    ("publications_info"
     "{\"PMID:27475717\": {\"publication date\": \"2016 09 15\", \"sentence\": \"In addition, ERK and GSK3beta-dependent C/EBPbeta phosphorylation was attenuated by andrographolide.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0051821"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0178601"
   "Dopamine Agonists"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0178601---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:26153447")
    ("publications_info"
     "{\"PMID:26153447\": {\"publication date\": \"2015 Oct\", \"sentence\": \"Pretreatment with a positive allosteric modulator (PAM) of muscarinic acetylcholine M4 receptors (M4Rs), VU0152100, attenuated the D1R agonist-stimulated ERK phosphorylation in the two regions, whereas the PAM itself did not alter basal ERK phosphorylation.\", \"subject score\": 819, \"object score\": 819}}")
    ("subject" "UMLS:C0178601"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0002932"
   "Anesthetics"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0002932---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:29186909")
    ("publications_info"
     "{\"PMID:29186909\": {\"publication date\": \"2017 Nov 27\", \"sentence\": \"Ketamine, a Clinically Used Anesthetic, Inhibits Vascular Smooth Muscle Cell Proliferation via PP2A-Activated PI3K/Akt/ERK Inhibition.\", \"subject score\": 1000, \"object score\": 741}}")
    ("subject" "UMLS:C0002932"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1449680"
   "Angiotensin II Type 1 Receptor Blockers"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1449680---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:28871467")
    ("publications_info"
     "{\"PMID:28871467\": {\"publication date\": \"2018 Feb\", \"sentence\": \"In addition, HASMC proliferation was suppressed by the addition of an AT-1R blocker (olmesartan), an ERK1/2 inhibitor (PD98059), and a p38MAPK inhibitor (SB202190).\", \"subject score\": 993, \"object score\": 827}}")
    ("subject" "UMLS:C1449680"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0962192"
   "Bay 11-7085"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0962192---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:26993765")
    ("publications_info"
     "{\"PMID:26993765\": {\"publication date\": \"2016 Apr 26\", \"sentence\": \"Inhibition of autophagy markedly decreased endogenous and BAY 11-7085-induced ERK phosphorylation, suggesting a positive feed back loop between ERK activation and autophagy in synovial fibroblasts.\", \"subject score\": 816, \"object score\": 816}}")
    ("subject" "UMLS:C0962192"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0243192"
   "agonists"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0243192---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:20553736|PMID:26993765|PMID:27551480")
    ("publications_info"
     "{\"PMID:27551480\": {\"publication date\": \"2015\", \"sentence\": \"Furthermore, levels of phosphor-TSC2 (Ser664) were increased after treatment with FVII and PAR2 agonist whereas these were significantly abolished in the presence of a potent and specific MEK/ERK inhibitor U0126.\", \"subject score\": 861, \"object score\": 729}, \"PMID:26993765\": {\"publication date\": \"2016 Apr 26\", \"sentence\": \"Both BAY 11-7085-induced autophagy and GR activation were down regulated with PPAR-gamma agonist, 15d-PGJ2 and MEK/ERK inhibitor UO126.\", \"subject score\": 888, \"object score\": 764}, \"PMID:20553736\": {\"publication date\": \"2010 Jul 03\", \"sentence\": \"The effect was synergistically augmented by PPARgamma agonist, but attenuated by inhibitors of PPARgamma, ERK or p38 MAPK.\", \"subject score\": 938, \"object score\": 1000}}")
    ("subject" "UMLS:C0243192"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0040616"
   "Anti-Anxiety Agents"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0040616---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:27876502|PMID:29137241")
    ("publications_info"
     "{\"PMID:29137241\": {\"publication date\": \"2017 Oct 10\", \"sentence\": \"CZ415 could be further tested as a promising anti-osteosarcoma agent, alone or in combination of ERK inhibition.\", \"subject score\": 844, \"object score\": 861}, \"PMID:27876502\": {\"publication date\": \"2017 Jan 04\", \"sentence\": \"CONCLUSION: The results indicate that OD as an anti-metastatic agent suppresses the metastatic response by targeting p-ERK, p-38 and NF-kappaB, thus reducing the invasion capacity of MCF-7 breast cancer cells through inhibition of MMP-9 and ICAM-1 expression and plays an important role in the regulation of breast cancer cell apoptosis.\", \"subject score\": 896, \"object score\": 790}}")
    ("subject" "UMLS:C0040616"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1373060"
   "Nitric Oxide Synthase Inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1373060---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:22160804")
    ("publications_info"
     "{\"PMID:22160804\": {\"publication date\": \"2012 Apr\", \"sentence\": \"The effects of iNOS inhibitors were associated with diminished ouabain tyrosine nitration as well as abrogation of ouabain-induced p38 and ERK phosphorylation.\", \"subject score\": 937, \"object score\": 861}}")
    ("subject" "UMLS:C1373060"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0003209"
   "Anti-Inflammatory Agents"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003209---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:21347512")
    ("publications_info"
     "{\"PMID:21347512\": {\"publication date\": \"2011 May\", \"sentence\": \"Aspirin, an anti-inflammatory drug, is a known ERK inhibitor and prevents neurodegenerative disorders including prion diseases.\", \"subject score\": 983, \"object score\": 785}}")
    ("subject" "UMLS:C0003209"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1171350"
   "kinase inhibitor [EPC]"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1171350---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:19127349|PMID:19890601")
    ("publications_info"
     "{\"PMID:19127349\": {\"publication date\": \"2009 Feb\", \"sentence\": \"The loss in countering capacity of leptin on the ethanol-induced cytotoxicity was attained with Src kinase inhibitor, PP2, and EGFR kinase inhibitor, AG1478, as well as ERK inhibitor, PD98059.\", \"subject score\": 901, \"object score\": 827}, \"PMID:19890601\": {\"publication date\": \"2010 Jul\", \"sentence\": \"The sole systemic therapy that has shown efficacy in improving the survival of HCC patients is sorafenib, an oral kinase inhibitor that blocks the Raf/MEK/ERK pathway and the receptor for VEGFR 2 and PDGFR-beta.\", \"subject score\": 901, \"object score\": 763}}")
    ("subject" "UMLS:C1171350"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0002932"
   "Anesthetics"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0002932---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:17492663")
    ("publications_info"
     "{\"PMID:17492663\": {\"publication date\": \"2007 Oct 01\", \"sentence\": \"Volatile anesthetics affect the morphology of rat glioma C6 cells via RhoA, ERK, and Akt activation.\", \"subject score\": 888, \"object score\": 1000}}")
    ("subject" "UMLS:C0002932"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C3543842"
   "Tonics"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C3543842---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:15327816")
    ("publications_info"
     "{\"PMID:15327816\": {\"publication date\": \"2004 Sep\", \"sentence\": \"The results from this study indicate that persistent ERK activation is required for the enhanced behavioral responses to spinal group I mGluR activation following inflammation and suggest that tonic modulation of ERK activity may underlie a component of central sensitization in dorsal horn neurons.\", \"subject score\": 661, \"object score\": 861}}")
    ("subject" "UMLS:C3543842"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1517132"
   "Farnesyl Transferase Inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1517132---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:16712893")
    ("publications_info"
     "{\"PMID:16712893\": {\"publication date\": \"2006 Sep 15\", \"sentence\": \"Both FTIs successfully inhibited the ERK and activated JNK in RIE/K-ras cells.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C1517132"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1579373"
   "Drugs used for the treatment of acute migraine"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1579373---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:16020508")
    ("publications_info"
     "{\"PMID:16020508\": {\"publication date\": \"2005 Nov 01\", \"sentence\": \"Thus, Tregs share biochemical characteristics of anergy, including abortive activation of Ras-MEK-Erk, increased activation of Rap1, and increased expression of p27kip1.\", \"subject score\": 694, \"object score\": 660}}")
    ("subject" "UMLS:C1579373"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0243042"
   "Inflammation Mediators"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0243042---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:15379598|PMID:22253793|PMID:30455267")
    ("publications_info"
     "{\"PMID:15379598\": {\"publication date\": \"2004 Sep\", \"sentence\": \"In particular, mitogen-activated protein kinase (MAPK), such as ERK and p38, is activated by inflammatory mediators in primary sensory and secondary order dorsal horn neurons and participates in the generation and maintenance of inflammatory pain.\", \"subject score\": 964, \"object score\": 1000}, \"PMID:22253793\": {\"publication date\": \"2012\", \"sentence\": \"Furthermore, TLR agonists or live pathogen (S. aureus, P. aeruginosa, & C. albicans)-challenged Muller glia produced significantly higher levels of inflammatory mediators (TNF-alpha, IL-1beta, IL-6 and IL-8), concomitantly with the activation of NF-kappaB, p38 and Erk signaling.\", \"subject score\": 964, \"object score\": 861}, \"PMID:30455267\": {\"publication date\": \"2018 Dec 03\", \"sentence\": \"Mechanistically, TLR8, localizing in the endosomes and lysosomes, mediated ERK activation, inflammatory mediators' production, and neuronal hyperexcitability after SNL.\", \"subject score\": 877, \"object score\": 627}}")
    ("subject" "UMLS:C0243042"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0304403"
   "Psychostimulant (substance)"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0304403---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:16954211")
    ("publications_info"
     "{\"PMID:16954211\": {\"publication date\": \"2006 Oct 27\", \"sentence\": \"In contrast, psychostimulants activate ERK and induce hyperactivity in normal animals.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0304403"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1519313"
   "Signal Transduction Inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1519313---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:15655409")
    ("publications_info"
     "{\"PMID:15655409\": {\"publication date\": \"2005 Feb\", \"sentence\": \"BAY 43-9006 (BAY) is a novel signal transduction inhibitor that prevents tumor cell proliferation and angiogenesis through blockade of the Raf/MEK/ERK pathway at the level of Raf kinase and the receptor tyrosine kinases vascular endothelial growth factor receptor-2 and platelet-derived growth factor receptor-beta.\", \"subject score\": 916, \"object score\": 763}}")
    ("subject" "UMLS:C1519313"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1514727"
   "Ras Inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1514727---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:12812842")
    ("publications_info"
     "{\"PMID:12812842\": {\"publication date\": \"2003 Jul 03\", \"sentence\": \"Ras inhibitor, Erk blocker or phosphatidylinositol 3-kinase inhibitor decreased depolarization- or NO donor-promoted survival.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C1514727"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1268567"
   "Protein-tyrosine kinase inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1268567---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:11978788|PMID:22773546|PMID:30258794")
    ("publications_info"
     "{\"PMID:11978788\": {\"publication date\": \"2002 Jun 28\", \"sentence\": \"In rat extensor digitorum longus (EDL) muscles, (a) AMPK activator, 5-aminoimidazole-4-carboxamide-1-beta-d-riboside (AICAR), activated PYK2, ERK and aPKCs; (b) effects of AICAR on ERK and aPKCs were blocked by tyrosine kinase inhibitor, genistein, and MEK1 inhibitor, PD98059; and (c) effects of AICAR on aPKCs and 2-deoxyglucose (2-DOG) uptake were inhibited by genistein, PD98059, and PLD-inhibitor, 1-butanol.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:30258794\": {\"publication date\": \"2018 Sep-Oct\", \"sentence\": \"The following groups of systemic drugs have been considered: taxanes, epidermal growth factor-receptor (EGFR) inhibitors, EGFR tyrosine kinase inhibitors, tyrosine kinase inhibitors, inhibitors of MEK/ERK, BRAF inhibitors, CD20 antagonists, vascular endothelial growth factor inhibitors, and retinoids.\", \"subject score\": 1000, \"object score\": 694}, \"PMID:22773546\": {\"publication date\": \"2013 Oct\", \"sentence\": \"In cells lines with complex activation profiles (HSC39 and OE33), a combination of TKIs or Mek inhibition (in nM concentrations) was necessary for cytotoxicity and inhibition of Erk and Akt phosphorylation.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C1268567"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1258619"
   "decursinol"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1258619---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:12677526")
    ("publications_info"
     "{\"PMID:12677526\": {\"publication date\": \"2003 Mar\", \"sentence\": \"G-Rd and DC attenuated, in part, the increased phospho-ERK and the decreased phospho-CREB protein levels.\", \"subject score\": 1000, \"object score\": 660}}")
    ("subject" "UMLS:C1258619"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1514555"
   "Protein Kinase C Inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1514555---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:12676927|PMID:12745093|PMID:20477948|PMID:20600853")
    ("publications_info"
     "{\"PMID:12676927\": {\"publication date\": \"2003 Jun 27\", \"sentence\": \"Inhibition of the ERK and protein kinase C signaling pathways with the MEK-1 inhibitor, U0126, and protein kinase C inhibitor, GF 1092030x, respectively, and chelating intracellular free calcium with 1,2-bis(2-aminophenoyl)ethane-N,N,N',N'-tetraacetic acid-AM, which also reduced ERK1/2 activation, significantly reduced H2O2-induced AA release in MC+/+ expressing either group IIa or V PLA2s.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:12745093\": {\"publication date\": \"2003 May 30\", \"sentence\": \"The PMA-induced MMP-9 secretion was abolished by treatment of a pan-protein kinase C (PKC) inhibitor, GF109203X, and an inhibitor of NF-kappaB activation, sulfasalazine, and partly inhibited by treatment of inhibitors of ERK pathway, PD98059 and U0126.\", \"subject score\": 937, \"object score\": 861}, \"PMID:20477948\": {\"publication date\": \"2010 Aug\", \"sentence\": \"In addition, protein kinase C inhibitors suppressed ATP-induced ERK and JNK activation, and also inhibited ATP-induced CXCL2 expression in microglia.\", \"subject score\": 983, \"object score\": 623}, \"PMID:20600853\": {\"publication date\": \"2010 Oct\", \"sentence\": \"The protein kinase C inhibitors, RO 320432 and GO 6983, and the ERK inhibitors UO 126 and PD 98059 all activated PDE4A4 aggregate formation, whilst roscovitine, thalidomide and the tyrosine kinase inhibitors, genistein and AG17, all inhibited this process.\", \"subject score\": 983, \"object score\": 824}}")
    ("subject" "UMLS:C1514555"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0003402"
   "Antioxidants"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003402---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:14647418")
    ("publications_info"
     "{\"PMID:14647418\": {\"publication date\": \"2004 Feb 19\", \"sentence\": \"Lastly, antioxidants (e.g., L-N-acetylcysteine; L-NAC) opposed adaphostin-mediated mitochondrial dysfunction, Raf-1/MEK/ERK downregulation, JNK activation, and apoptosis.\", \"subject score\": 1000, \"object score\": 794}}")
    ("subject" "UMLS:C0003402"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0597220"
   "phospholipase inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0597220---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:12759139")
    ("publications_info"
     "{\"PMID:12759139\": {\"publication date\": \"2003 Jun 13\", \"sentence\": \"Pertussis toxin, an inhibitor of G(i)/G(o) protein, and phospholipase C (PLC) inhibitor blocked Lkn-1-induced activation of ERK.\", \"subject score\": 890, \"object score\": 1000}}")
    ("subject" "UMLS:C0597220"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1513344"
   "Mitogen-Activated Protein Kinase Inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1513344---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications"
     "PMID:12403788|PMID:14592853|PMID:19878271|PMID:29777202|PMID:31807022")
    ("publications_info"
     "{\"PMID:12403788\": {\"publication date\": \"2003 Jan 10\", \"sentence\": \"SB203580, a p38 MAPK inhibitor, and PD98059, an ERK inhibitor, but not wortmannin a phosphatidylinositol 3-kinase (PI3K) inhibitor, prevented AA toxicity in pyrazole hepatocytes and E47 cells.\", \"subject score\": 938, \"object score\": 827}, \"PMID:19878271\": {\"publication date\": \"2010 Feb 01\", \"sentence\": \"Compared with PD98059, an MAPK inhibitor, baicalein exhibited a stronger inhibitory effect on Erk(1/2) phosphorylation.\", \"subject score\": 983, \"object score\": 1000}, \"PMID:29777202\": {\"publication date\": \"2018 May 18\", \"sentence\": \"In these resistant cells, phosphorylation of ribosomal protein S6 (rpS6) but not phosphorylation of ERK or p90 ribosomal S6 kinase (RSK) were unable to be inhibited by MAPK pathway inhibitors.\", \"subject score\": 901, \"object score\": 1000}, \"PMID:14592853\": {\"publication date\": \"2004 Jan\", \"sentence\": \"PD98059, a mitogen-activated protein kinase/ERK kinase inhibitor, or SB23058, a p38MAPK inhibitor, significantly attenuated the vasotrophic effect of nicotine and Ang II.\", \"subject score\": 823, \"object score\": 827}, \"PMID:31807022\": {\"publication date\": \"2019\", \"sentence\": \"Finally, ERK was inhibited using a mitogen-activated protein kinase/ERK kinase inhibitor (U0126) to further explore the molecular mechanism of GOF-mutant SHP2 affecting GBM cells.\", \"subject score\": 927, \"object score\": 1000}}")
    ("subject" "UMLS:C1513344"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1443775"
   "Epidermal growth factor receptor inhibitor (product)"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1443775---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications"
     "PMID:11948693|PMID:12402303|PMID:19277491|PMID:21904878|PMID:28791489")
    ("publications_info"
     "{\"PMID:11948693\": {\"publication date\": \"2002\", \"sentence\": \"Proliferation, activation of ERK, and biosynthesis of Egr-1 was completely inhibited in EGF or thrombin-treated HaCaT cells by the MAP kinase kinase inhibitor PD98059 and by AG1487, an EGF receptor-specific tyrosine kinase inhibitor.\", \"subject score\": 932, \"object score\": 1000}, \"PMID:12402303\": {\"publication date\": \"2002 Dec 01\", \"sentence\": \"Tyrphostin AG1478, an EGFR tyrosine kinase inhibitor, blocks ALP-induced MAPK/ERK activation but not EGFR internalization.\", \"subject score\": 1000, \"object score\": 562}, \"PMID:19277491\": {\"publication date\": \"2009 Sep\", \"sentence\": \"Rather, epidermal growth factor (EGF) receptor signaling mediated clozapine-induced ERK activation, given dose-dependent reduction of pERK1 and pERK2 stimulation with the EGF receptor inhibitor, AG1478.\", \"subject score\": 1000, \"object score\": 861}, \"PMID:21904878\": {\"publication date\": \"2011 Oct\", \"sentence\": \"Pretreatment of beta-cells with HNMPA, an insulin receptor inhibitor, and AG1478, an epidermal growth factor receptor inhibitor, further increased the cAMP level and Erk phosphorylation in the presence of exendin-4 (exe-4), a GLP-1 agonist.\", \"subject score\": 1000, \"object score\": 861}, \"PMID:28791489\": {\"publication date\": \"2017 Nov\", \"sentence\": \"Sustained activation of ERK by overexpression of constitutively active MEK1 was sufficient to expand CD44+/CD24- populations in cells in which EGFR activity was blocked by either erlotinib, an EGFR kinase inhibitor, or BB-94, a metalloprotease inhibitor that prevents generation of soluble EGFR ligands.\", \"subject score\": 938, \"object score\": 1000}}")
    ("subject" "UMLS:C1443775"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0597217"
   "phosphatase inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0597217---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:16464862")
    ("publications_info"
     "{\"PMID:16464862\": {\"publication date\": \"2006 Apr 21\", \"sentence\": \"In contrast, p38 MAPK inhibitors had no detectable effect on the ERK activation induced by fibroblast growth factor 2 or pervanadate, a phosphatase inhibitor, and MEK inhibitors did not influence p38 MAPK phosphorylation, confirming both the specificity and unidirectionality of p38 MAPK-ERK cross-talk.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0597217"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1513344"
   "Mitogen-Activated Protein Kinase Inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1513344---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:11466422|PMID:11684667|PMID:14701813|PMID:31776277")
    ("publications_info"
     "{\"PMID:11466422\": {\"publication date\": \"2001 Aug 01\", \"sentence\": \"PD98059, a selective MAPK kinase inhibitor, had a smaller and only marginally significant effect on CR acquisition, although it did block the learning-related increases in ERK activity in both the hippocampus and anterior vermis.\", \"subject score\": 911, \"object score\": 861}, \"PMID:14701813\": {\"publication date\": \"2004 Mar 12\", \"sentence\": \"JNK, p38, and ERK activation seem not to be required for this type of cell death because mitogen-activated protein kinase inhibitors did not significantly affect TNF-induced necrotic cell death.\", \"subject score\": 988, \"object score\": 1000}, \"PMID:11684667\": {\"publication date\": \"2001 Nov\", \"sentence\": \"We find that Erk MAP kinase is normally active in ureteric bud, and that inhibiting Erk activation with the MAP kinase kinase inhibitor, PD98059, reversibly inhibits branching in a dose-dependent manner, while allowing tubule elongation to continue.\", \"subject score\": 941, \"object score\": 627}, \"PMID:31776277\": {\"publication date\": \"2019 Nov 27\", \"sentence\": \"Using MAPK inhibitors, we found that LMP1 activates ERK or p38 to repress the expression of DUSP6 and DUSP8, with corresponding substrate specificity.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C1513344"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0085387"
   "Cyclooxygenase Inhibitors"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0085387---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:16458997")
    ("publications_info"
     "{\"PMID:16458997\": {\"publication date\": \"2006 Jul\", \"sentence\": \"Both the cyclooxygenase inhibitor, indomethacin and the ERK inhibitors, UO126 and PD980589 reverse the hypoxia-induced increase in intracellular cAMP levels back to those seen in normoxic hPASM cells.\", \"subject score\": 1000, \"object score\": 861}}")
    ("subject" "UMLS:C0085387"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0600437"
   "Nitric Oxide Donors"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0600437---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:10744637")
    ("publications_info"
     "{\"PMID:10744637\": {\"publication date\": \"2000 Apr\", \"sentence\": \"The STE-induced increase in phospho-ERK was suppressed by NO donors and the cGMP mimetic, and reversed by cGMP-PKG inhibitor, as was expression of AML1B and DNA binding in nuclear extracts.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0600437"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1443643"
   "Proteasome inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1443643---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:10617109")
    ("publications_info"
     "{\"PMID:10617109\": {\"publication date\": \"2000 Jan\", \"sentence\": \"Uniquely, the kinetics of MAP kinase activation induced by proteasome inhibitors are very slow compared with those resulting from activation by nerve growth factor; ERK activation is detectable only after a 5-h treatment with the inhibitors, and its activity remained unchanged for at least until 27 h.\", \"subject score\": 983, \"object score\": 1000}}")
    ("subject" "UMLS:C1443643"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1514555"
   "Protein Kinase C Inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1514555---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:10523856|PMID:15863357|PMID:22561846")
    ("publications_info"
     "{\"PMID:10523856\": {\"publication date\": \"1999 Oct 14\", \"sentence\": \"The MEK inhibitor PD 098059, as well as the PKC inhibitors, completely blocked TPA-mediated ERK activation.\", \"subject score\": 983, \"object score\": 547}, \"PMID:15863357\": {\"publication date\": \"2005 Apr 15\", \"sentence\": \"DMS had no effect on the dbcAMP-induced membrane translocation of protein kinase C (PKC) isozymes, and PKC inhibitors had no significant effect on ERK activation.\", \"subject score\": 983, \"object score\": 1000}, \"PMID:22561846\": {\"publication date\": \"2012 Aug\", \"sentence\": \"The protein kinase C inhibitor, Ro-31-7459, is a potent activator of ERK and JNK MAP kinases in HUVECs and yet inhibits cyclic AMP-stimulated SOCS-3 gene induction through inactivation of the transcription factor c-Jun.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C1514555"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1268567"
   "Protein-tyrosine kinase inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1268567---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:9933031")
    ("publications_info"
     "{\"PMID:9933031\": {\"publication date\": \"1999 Feb 15\", \"sentence\": \"On the other hand, an obligatory tyrosine phosphorylation step for activation of ERK was indicated by the use of protein tyrosine kinase inhibitors, which profoundly inhibited the activation of ERK by EGF, Ang II, and PMA.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C1268567"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0162754"
   "Serotonin 5-HT1 Receptor Agonists"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0162754---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:9658404")
    ("publications_info"
     "{\"PMID:9658404\": {\"publication date\": \"1998 Jul\", \"sentence\": \"We further demonstrate that 5-HT1 agonists inactivate ERK by dephosphorylation, even in the presence of constitutively activated MEK1.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0162754"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1514727"
   "Ras Inhibitor"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1514727---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:16052566|PMID:8969227")
    ("publications_info"
     "{\"PMID:8969227\": {\"publication date\": \"1996 Dec 27\", \"sentence\": \"Although pretreatment with manumycin, a Ras farnesyltransferase inhibitor, or overexpression of a dominant-negative mutant of Ras inhibited insulin-induced ERK activation, neither affected AngII-induced activation of ERKs.\", \"subject score\": 901, \"object score\": 583}, \"PMID:16052566\": {\"publication date\": \"2005 Sep 15\", \"sentence\": \"Our data also indicated that ERK activation and the potentiation of ATP calcium responses were sensitive to the src-like kinase inhibitor herbimycin A, p21(ras) farnesyltransferase inhibitor peptide, and some PKC inhibitors.\", \"subject score\": 861, \"object score\": 1000}}")
    ("subject" "UMLS:C1514727"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1171350"
   "kinase inhibitor [EPC]"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1171350---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:20149136|PMID:28197555")
    ("publications_info"
     "{\"PMID:28197555\": {\"publication date\": \"2017 Jan-Feb\", \"sentence\": \"This activation was blocked by a MAPK kinase inhibitor, suggesting that similar pathways are involved in activation of ERK and p38 MAPK.\", \"subject score\": 901, \"object score\": 1000}, \"PMID:20149136\": {\"publication date\": \"2010 Apr\", \"sentence\": \"PLX4032, a selective BRAF(V600E) kinase inhibitor, activates the ERK pathway and enhances cell migration and proliferation of BRAF melanoma cells.\", \"subject score\": 1000, \"object score\": 861}}")
    ("subject" "UMLS:C1171350"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0040615"
   "Antipsychotic Agents"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0040615---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:24203573")
    ("publications_info"
     "{\"PMID:24203573\": {\"publication date\": \"2014 Mar\", \"sentence\": \"Taken together, our data suggest that (+/-)-alpha-lipoic acid exerts synergistic effects with haloperidol on the Akt/GSK-3beta pathway, potentially involved in the therapeutic effects of APs, and antagonism of ERK activation and D2R upregulation, potentially involved in tardive dyskinesia and treatment resistance.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0040615"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0003289"
   "Antidepressants"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003289---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:25476565|PMID:30692946")
    ("publications_info"
     "{\"PMID:25476565\": {\"publication date\": \"2015 Mar 01\", \"sentence\": \"Results demonstrate that alarin may exert antidepressant-like effects by targeting TrkB receptor-mediated ERK and AKT signal systems, which could help to identify the alarin receptor.\", \"subject score\": 1000, \"object score\": 623}, \"PMID:30692946\": {\"publication date\": \"2018\", \"sentence\": \"Blockade of the ERK-CREB axis with the ERK-specific inhibitor U0126 repressed the neuroprotective and antidepressant-like effects of paeoniflorin on rats in the setting of chronic-mild-stress and abolished the recoveries of p-ERK mediated by paeoniflorin treatment.\", \"subject score\": 1000, \"object score\": 861}}")
    ("subject" "UMLS:C0003289"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0003289"
   "Antidepressants"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003289---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:25476565|PMID:27634096|PMID:31768875")
    ("publications_info"
     "{\"PMID:25476565\": {\"publication date\": \"2015 Mar 01\", \"sentence\": \"This resulted in an absence of antidepressant-like effects, as well as no activation of ERK and AKT signaling pathways.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:27634096\": {\"publication date\": \"2016 12\", \"sentence\": \"Together, the results demonstrate that these three different rapid acting antidepressant agents increase ERK signaling and BDNF release in an activity dependent manner that leads to increased neuronal complexity.\", \"subject score\": 748, \"object score\": 861}, \"PMID:31768875\": {\"publication date\": \"2019 Nov 25\", \"sentence\": \"The antidepressant-like effect of guanosine is dependent on GSK-3beta inhibition and activation of MAPK/ERK and Nrf2/heme oxygenase-1 signaling pathways.\", \"subject score\": 1000, \"object score\": 694}}")
    ("subject" "UMLS:C0003289"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0013036"
   "Dopaminergic Agents"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0013036---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:24380761")
    ("publications_info"
     "{\"PMID:24380761\": {\"publication date\": \"2014 Mar\", \"sentence\": \"The selective increase in ERK activity in the PFC associated with behavioral sensitization, points to a possible pivotal role of the dopamine projection to the medial frontal cortex in the mediation of neural plasticity, considered to underlie the sensitization processes induced by dopaminergic drugs.\", \"subject score\": 1000, \"object score\": 861}}")
    ("subject" "UMLS:C0013036"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0013227"
   "Pharmaceutical Preparations"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0013227---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications"
     "PMID:11170175|PMID:12151388|PMID:12163544|PMID:14697665|PMID:19683494|PMID:20668238|PMID:21645515|PMID:24348046|PMID:27614430|PMID:29291014|PMID:31649535")
    ("publications_info"
     "{\"PMID:24348046\": {\"publication date\": \"2013\", \"sentence\": \"Additionally, such a combination is expected to reduce MEKi-induced skin toxicities, as these drugs are thought to have antagonistic effects on ERK activation in keratinocytes.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:11170175\": {\"publication date\": \"2001 Feb 01\", \"sentence\": \"Doxorubicin (0.2 microM), an anticancer drug whose mechanism of action is independent of microtubules, also induced ERK activation, tau phosphorylation and apoptosis.\", \"subject score\": 861, \"object score\": 645}, \"PMID:12151388\": {\"publication date\": \"2002 Oct 04\", \"sentence\": \"Hence, anti-calmodulin drugs (such as W13, trifluoroperazine, or W7) are able to induce Ras/ERK pathway activation under low levels of growth factors.\", \"subject score\": 790, \"object score\": 645}, \"PMID:12163544\": {\"publication date\": \"2002 Aug\", \"sentence\": \"These drugs also inhibit TGFbeta1 effects on Akt/PKB phosphorylation but have no effect on TGFbeta1-evoked Erk activation.\", \"subject score\": 1000, \"object score\": 583}, \"PMID:14697665\": {\"publication date\": \"2003 Dec\", \"sentence\": \"Furthermore, glial cell elimination or inactivation in the culture, by gliotoxic drugs, abrogates NO-induced ERK activation.\", \"subject score\": 861, \"object score\": 619}, \"PMID:19683494\": {\"publication date\": \"2009 Aug 14\", \"sentence\": \"Upregulation of Mig6 by lipid agonists such as LPA and S1P or actin drugs involved MAL and correlated with decreased activation of EGFR, MAPK/Erk, and c-fos.\", \"subject score\": 888, \"object score\": 694}, \"PMID:21645515\": {\"publication date\": \"2011 Aug 15\", \"sentence\": \"Nevertheless, RalA was not only cytoprotective against multiple chemotherapeutic drugs, but also promigratory inducing stress fiber formation, which was accompanied by the activation of Akt and Erk pathways.\", \"subject score\": 828, \"object score\": 1000}, \"PMID:29291014\": {\"publication date\": \"2017 Dec 05\", \"sentence\": \"The pattern of upregulated proteins was similar with both drugs and indicates an activated RAF/MEK/ERK pathway, but more proteins were downregulated with sorafenib versus regorafenib.\", \"subject score\": 1000, \"object score\": 742}, \"PMID:27614430\": {\"publication date\": \"2016 Dec\", \"sentence\": \"Therefore, vanadium compounds can be regarded as a novel type of anticancer drugs through the prolonged activation of MAPK/ERK pathway but retained AKT activity.\", \"subject score\": 861, \"object score\": 802}, \"PMID:20668238\": {\"publication date\": \"2010 Aug 17\", \"sentence\": \"In contrast, the drug activates MEK and ERK phosphorylation in cells with wild-type BRAF.\", \"subject score\": 1000, \"object score\": 861}, \"PMID:31649535\": {\"publication date\": \"2019\", \"sentence\": \"Activations of Akt or ERK pathway induced by clinical drugs promote therapeutic failure due to decrease of drug response, and no available strategies have been developed to solve these problems.\", \"subject score\": 888, \"object score\": 861}}")
    ("subject" "UMLS:C0013227"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0243192"
   "agonists"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0243192---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications"
     "PMID:10026227|PMID:10187821|PMID:10537288|PMID:10760082|PMID:12706479|PMID:15149321|PMID:15153488|PMID:15632168|PMID:15944153|PMID:16278303|PMID:16709153|PMID:17312106|PMID:18261471|PMID:19347570|PMID:19501623|PMID:19662020|PMID:21541969|PMID:22306083|PMID:22796106|PMID:23750009|PMID:23991110|PMID:24140231|PMID:24151242|PMID:26107116|PMID:27349565|PMID:27456816|PMID:31266805|PMID:9687510|PMID:9870938|PMID:9933031|PMID:9990315")
    ("publications_info"
     "{\"PMID:24151242\": {\"publication date\": \"2013 Dec 01\", \"sentence\": \"However, this phenotype is most obvious on a G(389)-beta1AR background; the more robust agonist-dependent cAMP/PKA and ERK responses in R(389)-beta1AR cells effectively obscure the effect of the S49G polymorphism.\", \"subject score\": 740, \"object score\": 861}, \"PMID:23991110\": {\"publication date\": \"2013\", \"sentence\": \"Agonist-induced Erk phosphorylation was preceded by rapid FGFR and EGFR transactivation; however, only EGFR inhibition blocked Erk activation and proliferation.\", \"subject score\": 775, \"object score\": 775}, \"PMID:23750009\": {\"publication date\": \"2013 Aug 15\", \"sentence\": \"The effects of ARRBs were isoform specific; ARRB2 inhibited MC1R agonist-dependent cAMP production but not ERK activation, stimulated internalization and showed prolonged co-localization with the receptor in endocytic vesicles.\", \"subject score\": 799, \"object score\": 660}, \"PMID:24140231\": {\"publication date\": \"2013 Nov 15\", \"sentence\": \"Agonist-induced thromboxane A2 (TxA2) generation and ERK phosphorylation were significantly inhibited by TBCA.\", \"subject score\": 833, \"object score\": 861}, \"PMID:9687510\": {\"publication date\": \"1998 Aug 03\", \"sentence\": \"In HeLa, PC12 and SK-N-MC cells, PD 98059 and SB 203580 are both required to suppress the activation of MSK1 by TNF, NGF and FGF, respectively, because these agonists activate both the MAPK/ERK and SAPK2/p38 cascades.\", \"subject score\": 1000, \"object score\": 694}, \"PMID:10026227\": {\"publication date\": \"1999 Feb 26\", \"sentence\": \"Together, these results strongly suggest a role for redox-sensitive mechanisms in agonist-induced ERK2, JNK1, and p38 MAP kinase activation; c-Fos, c-Jun, and JunB expression; AP-1 activity; and DNA synthesis in VSMC.\", \"subject score\": 851, \"object score\": 1000}, \"PMID:9990315\": {\"publication date\": \"1998 Dec 15\", \"sentence\": \"TPO by itself did not activate ERK1, ERK2 and protein kinase C (PKC), whereas TPO directly enhanced the PKC-dependent activation of ERKs induced by other agonists including thrombin and phorbol esters, without affecting the PKC activation by those agonists.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:9933031\": {\"publication date\": \"1999 Feb 15\", \"sentence\": \"Our results show that although activation of PKC was critical for mitogenesis induced by Ang II or EGF, the initial activation of ERK by both agonists in these cells was essentially independent of PKC activation and was insensitive to Ca2+ mobilization.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:9870938\": {\"publication date\": \"1999 Jan 01\", \"sentence\": \"Parallel immunofluorescence confocal microscopy, using an anti-ERK antibody, showed that agonist-induced time-dependent ERK IR trafficking into perinuclear and nuclear loci was impaired in the internalization-defective cells.\", \"subject score\": 763, \"object score\": 763}, \"PMID:10187821\": {\"publication date\": \"1999 Apr 09\", \"sentence\": \"Point mutations within highly conserved regions of TM6 and intracellular loop 3 were without effect on agonist-stimulated ERK activation.\", \"subject score\": 645, \"object score\": 645}, \"PMID:10537288\": {\"publication date\": \"1999 Oct 15\", \"sentence\": \"Elimination of extracellular Ca2+ by EGTA also did not abolish the activation of ERK by GnRHa.\", \"subject score\": 916, \"object score\": 1000}, \"PMID:12706479\": {\"publication date\": \"2003 Apr 25\", \"sentence\": \"Yohimbine and 2-(2,3-dihydro-2-methoxy-1,4-benzodioxin-2-yl)4,5-dihydro-1H-imidazole (RX821002), alpha(2)-adrenoceptor antagonists, significantly blocked agonist-induced interleukin-12 production and p38 MAPK activation, indicating that the effects of the agonists were mediated through alpha(2)-adrenoceptor.\", \"subject score\": 781, \"object score\": 1000}, \"PMID:15944153\": {\"publication date\": \"2005 Jul 29\", \"sentence\": \"The mu agonist, [D-ala2,mephe4,glyol5]enkephalin (DAMGO), induces a transient stimulation of ERK phosphorylation, whereas kappa agonist, U69,593, engenders sustained ERK activation.\", \"subject score\": 888, \"object score\": 660}, \"PMID:18261471\": {\"publication date\": \"2008 Mar\", \"sentence\": \"This calcilytic also attenuated CaR agonist-induced ERK activation in these cells.\", \"subject score\": 520, \"object score\": 520}, \"PMID:17312106\": {\"publication date\": \"2007 Mar 01\", \"sentence\": \"BBPs did not inhibit activation of ERK induced by P2C, a TLR2/6 agonist.\", \"subject score\": 802, \"object score\": 1000}, \"PMID:15153488\": {\"publication date\": \"2004 Jun 01\", \"sentence\": \"ERK is also activated by CCR2 agonists, e.g., monocyte chemoattractant protein-1 (CCL2).\", \"subject score\": 861, \"object score\": 1000}, \"PMID:15632168\": {\"publication date\": \"2005 Mar 04\", \"sentence\": \"These results show that specific kinetics of ERK activation by agonists and dual efficacy ligands are determined, at least in part, by the differential ability of the two types of drugs to trigger mechanisms regulating deltaOR responsiveness.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:16709153\": {\"publication date\": \"2006 Sep 01\", \"sentence\": \"More fundamentally, these observations shed new light on enigmatic issues such as the inefficacy of S17N-Ras in blocking PE action or the role of the EGFR in heterologous agonist activation of the Ras/ERK pathway.\", \"subject score\": 623, \"object score\": 827}, \"PMID:22306083\": {\"publication date\": \"2012 May 15\", \"sentence\": \"We further investigated the apoptotic mechanism demonstrating that E2, as well as ESR1 and GPER specific agonists, induced sustained ERK, c-Jun and p38 phosphorylation, Cytochrome c release, caspase 3 and endogenous substrate Poly (ADP-ribose) polymerase (PARP) activation and increased expression of cell cycle inhibitor p21.\", \"subject score\": 827, \"object score\": 694}, \"PMID:22796106\": {\"publication date\": \"2012 Nov\", \"sentence\": \"Blockade of glutamate release achieved by the mGluR2/3 agonist, LY354740 or the selective adenosine A1R agonist, CCPA as well as neurotoxic lesions of lateral entorhinal cortex reduced the ability of SKF81297 to induce ERK activation in the dentate gyrus.\", \"subject score\": 802, \"object score\": 1000}, \"PMID:19662020\": {\"publication date\": \"2009 Oct\", \"sentence\": \"Furthermore, the inverse agonist activity olmesartan exerts against stretch-induced ERK activation requires an additional drug-receptor interaction involving the tetrazole group of olmesartan and Gln(257) of the AT(1) receptor.\", \"subject score\": 833, \"object score\": 566}, \"PMID:19501623\": {\"publication date\": \"2009 Dec\", \"sentence\": \"Thus, in intact brain HB-EGF, known to be expressed in brain, may be the major EGF agonist released in response to stimulation of alpha(2)-adrenoceptors, the released agonist(s) activate(s) EGF receptors, and ERK(1/2) is phosphorylated as a conventional response to EGF receptor activation.\", \"subject score\": 888, \"object score\": 1000}, \"PMID:27456816\": {\"publication date\": \"2016 07 26\", \"sentence\": \"We also demonstrate the ability of these new agonists to induce receptor internalization, ERK activation, and chemotaxis, all hallmarks of CXCR4 activation.\", \"subject score\": 888, \"object score\": 1000}, \"PMID:26107116\": {\"publication date\": \"2015 Oct\", \"sentence\": \"The TRHR1 agonist, taltirelin, injection in the BLA increased the level of p-ERK, which mimicked the increased p-ERK level in the BLA that was induced by treatment with repeated stress.\", \"subject score\": 861, \"object score\": 861}, \"PMID:27349565\": {\"publication date\": \"2016 Oct 10\", \"sentence\": \"To our knowledge this is the first report of ERK and JNK activation in MCF10A and MCF12A cells with P4, P4 metabolites, MPA, and MPR-Ag.\", \"subject score\": 861, \"object score\": 1000}, \"PMID:19347570\": {\"publication date\": \"2009 May\", \"sentence\": \"PPAR gamma partial agonist, KR-62776, inhibits adipocyte differentiation via activation of ERK.\", \"subject score\": 851, \"object score\": 1000}, \"PMID:16278303\": {\"publication date\": \"2006 Mar 01\", \"sentence\": \"ANXA1 and Ac2-26 acted as genuine agonists; Ac2-26 binding led to ERK activation in both FPR- and FPRL-1/ALX-transfected cells, while ANXA1 caused ERK activation only in cells transfected with FPRL-1/ALX.\", \"subject score\": 853, \"object score\": 1000}, \"PMID:15149321\": {\"publication date\": \"2004 Jun\", \"sentence\": \"The role of ERK and phosphorylation in PPAR gamma activation were studied, as were the effects of PPAR agonists on ERK activation and cell proliferation.\", \"subject score\": 888, \"object score\": 1000}, \"PMID:21541969\": {\"publication date\": \"2011 Jun 15\", \"sentence\": \"GnRH receptor levels correlated with induction of inositol phosphates, elevation of intracellular Ca(2+) , cytoskeletal actin reorganization, modulation of ERK activation and cell growth-inhibition with GnRH agonists.\", \"subject score\": 888, \"object score\": 1000}, \"PMID:10760082\": {\"publication date\": \"2000 Apr\", \"sentence\": \"In contrast to ADPKD, proliferation and ERK activity of cells derived from normal HKC were not stimulated by cAMP agonists, although electrogenic Cl- secretion was increased by these agonists in both ADPKD and HKC cell monolayers.\", \"subject score\": 802, \"object score\": 861}, \"PMID:31266805\": {\"publication date\": \"2019 Aug 16\", \"sentence\": \"We conclude that PTPN7 regulates platelet functional responses downstream of GPCR agonists, but not GPVI agonists, through inhibition of ERK activation and thromboxane generation.\", \"subject score\": 928, \"object score\": 1000}}")
    ("subject" "UMLS:C0243192"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C1443775"
   "Epidermal growth factor receptor inhibitor (product)"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C1443775---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications"
     "PMID:16211241|PMID:18636178|PMID:19127349|PMID:19154417|PMID:19854188|PMID:22033246|PMID:24021351")
    ("publications_info"
     "{\"PMID:24021351\": {\"publication date\": \"2013 Nov 15\", \"sentence\": \"The EGFR inhibitor also inhibited the B[a]PDE-induced MEK/ERK and Akt signaling pathways and subsequently, suppressed COX-2 expression and promoter activity, in addition to suppressing the transactivation of AP-1 and NF-kappaB.\", \"subject score\": 1000, \"object score\": 549}, \"PMID:16211241\": {\"publication date\": \"2005 Nov\", \"sentence\": \"Paclitaxel-induced ERK and AKT activity was inhibited by the EGFR inhibitor, PD153035; ERK inhibitor, U0126; and PI3 kinase inhibitor, LY294002, respectively.\", \"subject score\": 1000, \"object score\": 827}, \"PMID:18636178\": {\"publication date\": \"2008 Aug\", \"sentence\": \"PD153035, an EGFR inhibitor, and U0126, an ERK inhibitor, inhibit atRA-induced upregulation of AQP3.\", \"subject score\": 1000, \"object score\": 827}, \"PMID:19154417\": {\"publication date\": \"2009 Mar\", \"sentence\": \"In these three high-sensitivity cells, the ERK pathway was activated without ligand stimulation, which was inhibited by EGFR TKI.\", \"subject score\": 1000, \"object score\": 861}, \"PMID:19127349\": {\"publication date\": \"2009 Feb\", \"sentence\": \"The loss in countering capacity of leptin on the ethanol-induced cytotoxicity was attained with Src kinase inhibitor, PP2, and EGFR kinase inhibitor, AG1478, as well as ERK inhibitor, PD98059.\", \"subject score\": 938, \"object score\": 827}, \"PMID:22033246\": {\"publication date\": \"2012 Jan 28\", \"sentence\": \"EGF induced the phosphorylation of EGFR, smad3, ERK, and JNK, and MMP-9 expression was decreased by the EGFR inhibitor, AG1478.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:19854188\": {\"publication date\": \"2009 Nov 19\", \"sentence\": \"Signal analyses showed that evodiamine stimulated the phosphorylation of EGFR, PKCalpha, and ERK, all of which were reduced by an EGFR inhibitor.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C1443775"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0243042"
   "Inflammation Mediators"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0243042---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:25501336")
    ("publications_info"
     "{\"PMID:25501336\": {\"publication date\": \"2014 Dec 09\", \"sentence\": \"These results suggest that pheophytin a functions by down-regulating the transcriptional levels of inflammatory mediators and blocking the ERK and STAT-1 pathways.\", \"subject score\": 964, \"object score\": 1000}}")
    ("subject" "UMLS:C0243042"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0003211"
   "Anti-Inflammatory Agents, Non-Steroidal"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003211---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:25406016")
    ("publications_info"
     "{\"PMID:25406016\": {\"publication date\": \"2015 Feb\", \"sentence\": \"In growth plate chondrocytes, inhibition of proliferation and ERK activation by NSAIDs is reversed by forskolin, 8-bromoadenosine, 3',5'-cAMP and a prostacyclin analog, iloprost.\", \"subject score\": 988, \"object score\": 1000}}")
    ("subject" "UMLS:C0003211"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0003402"
   "Antioxidants"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0003402---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications"
     "PMID:12682429|PMID:15698432|PMID:21344382|PMID:24534200|PMID:25485998|PMID:27916558|PMID:30481789")
    ("publications_info"
     "{\"PMID:24534200\": {\"publication date\": \"2014 Sep 01\", \"sentence\": \"Moreover, ERK activation induced by PFHxS was blocked by MK801 but not antioxidants.\", \"subject score\": 888, \"object score\": 1000}, \"PMID:25485998\": {\"publication date\": \"2015 Apr\", \"sentence\": \"Several studies have suggested that ERK activation in lung cells has a protective effect in response to hyperoxia, through stimulation of DNA repair and antioxidant mechanisms, and prolonged cell survival.\", \"subject score\": 888, \"object score\": 1000}, \"PMID:27916558\": {\"publication date\": \"2017 Feb 05\", \"sentence\": \"Therefore, the cardioprotective effects of CAR may be attributed to its antioxidant and antiapoptotic activities through activations of the MAPK/ERK and Akt/eNOS signaling pathways.\", \"subject score\": 1000, \"object score\": 694}, \"PMID:12682429\": {\"publication date\": \"2003 Feb\", \"sentence\": \"To determine whether reactive oxygen species (ROS) play a role in mediating ERK activation and 6-OHDA toxicity, we examined the effects of catalase, superoxide dismutase (SOD1), and metalloporphyrin antioxidants ('SOD mimetics') on 6-OHDA-treated cells.\", \"subject score\": 888, \"object score\": 1000}, \"PMID:21344382\": {\"publication date\": \"2011 Dec\", \"sentence\": \"Our experimental results clearly demonstrated that induction of p-ERK and cell proliferation by arsenite is mediated via oxidative stress, since antioxidants can inhibit arsenite-induced cell transformation.\", \"subject score\": 1000, \"object score\": 861}, \"PMID:30481789\": {\"publication date\": \"2018 Nov 27\", \"sentence\": \"All the compounds reduced p38MAPK activation, but only the AEM, especially ebselen, and NAC, both potentiating the glutathione peroxidase pathway, also inhibited NFkB activation.\", \"subject score\": 851, \"object score\": 1000}, \"PMID:15698432\": {\"publication date\": \"2005 Mar\", \"sentence\": \"Activation of ERK in renal fibrosis after unilateral ureteral obstruction: modulation by antioxidants.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0003402"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0013162"
   "Drug Combinations"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0013162---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:23501104|PMID:25704960|PMID:28649441")
    ("publications_info"
     "{\"PMID:23501104\": {\"publication date\": \"2013 Apr 19\", \"sentence\": \"The enhanced therapeutic efficacy of the drug combination was achieved by a relatively transient blockade of the ERK pathway.\", \"subject score\": 966, \"object score\": 861}, \"PMID:28649441\": {\"publication date\": \"2017\", \"sentence\": \"Prospective simulations were then used to evaluate potential drug combinations and predictive biomarkers for increasing responsiveness to MEK/ERK inhibitors in these patients.\", \"subject score\": 901, \"object score\": 802}, \"PMID:25704960\": {\"publication date\": \"2015 Sep\", \"sentence\": \"The drug combination inactivated ERK, AKT, p70 S6K, and mTOR and activated JNK.\", \"subject score\": 966, \"object score\": 1000}}")
    ("subject" "UMLS:C0013162"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0026249"
   "Mitogens"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0026249---SEMMEDDB:stimulates---None---None---increased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:18334532|PMID:20030620|PMID:24212659")
    ("publications_info"
     "{\"PMID:24212659\": {\"publication date\": \"2011 Mar 08\", \"sentence\": \"They also completely block the activity of mitogens such as epidermal growth factor's ability to stimulate ERK and Ras.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:18334532\": {\"publication date\": \"2008 May\", \"sentence\": \"In vivo, recruitment of ERK and MSK is stimulated by mitogens, correlates with histone H3 phosphorylation and is impaired by Elk-1 knockdown.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:20030620\": {\"publication date\": \"2010\", \"sentence\": \"They also completely block the activity of mitogens such as epidermal growth factor's ability to stimulate ERK.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "UMLS:C0026249"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594")))
  ("UMLS:C0013227"
   "Pharmaceutical Preparations"
   "biolink:regulates"
   "NCBIGene:5594"
   "MAPK1"
   "biolink:regulates"
   "UMLS:C1417707"
   "NFIX gene"
   (("id"
     "UMLS:C0013227---SEMMEDDB:inhibits---None---None---decreased---NCBIGene:5594---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "NCBIGene:5594")
    ("predicate" "biolink:regulates")
    ("publications"
     "PMID:11408604|PMID:12543078|PMID:15843040|PMID:18223169|PMID:21251612|PMID:21879255|PMID:24559688|PMID:30482227")
    ("publications_info"
     "{\"PMID:24559688\": {\"publication date\": \"2014 May\", \"sentence\": \"The sensitivity to the drug was accompanied by a potent inhibition of both phospho-ERK and phospho-AKT, and a significant induction of apoptosis while absent in lines with intrinsic or acquired resistance.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:11408604\": {\"publication date\": \"2001 Jul\", \"sentence\": \"The Src-family tyrosine kinase inhibitor PP2 blocked MAPK activation by dopamine; however, this drug was also found to inhibit PDGF-BB-stimulated ERK activity and autophosphorylation of the PDGF receptor-beta.\", \"subject score\": 1000, \"object score\": 872}, \"PMID:12543078\": {\"publication date\": \"2002 Nov 24\", \"sentence\": \"Interestingly, all of the effective drugs inhibited the ERK activity, while the drugs had no effects on p38 MAPK activity and IkappaB degradation.\", \"subject score\": 888, \"object score\": 861}, \"PMID:18223169\": {\"publication date\": \"2008 Apr 01\", \"sentence\": \"Finally, CD38 stimulation of T2 B lymphocytes obtained from Btk-, Lyn-, or Fyn-deficient mice showed a defective differentiation; similarly, drugs interfering with PI3K or ERK decreased the proliferation or differentiation of this subset.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:15843040\": {\"publication date\": \"2005 Apr 15\", \"sentence\": \"While ERK1/2 activation was a general phenomenon, irrespective of the used cell type or antitumour drug, the MEK/ERK inhibitors only reduced cisplatin toxicity in human myeloid cells (THP-1, HL-60 and NB-4), but not in RAW 264.7 mouse macrophages and NRK-52E rat renal tubular cells; and failed to reduce the toxicity etoposide, camptothecin, melphalan and arsenic trioxide, in U-937 cells.\", \"subject score\": 861, \"object score\": 753}, \"PMID:21879255\": {\"publication date\": \"2012 Jan\", \"sentence\": \"Neither (ZEGFR:1907)2 nor any of the other             drugs were able to completely inactivate Akt or Erk.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:21251612\": {\"publication date\": \"2011 Jan 18\", \"sentence\": \"These drugs thus selectively inhibit ERK signaling in tumors with BRAF mutation.\", \"subject score\": 861, \"object score\": 861}, \"PMID:30482227\": {\"publication date\": \"2018 Nov 27\", \"sentence\": \"BACKGROUND: Drugs that inhibit the MEK/ERK pathway have therapeutic benefit in bladder cancer treatment but responses vary with patients, for reasons that are still not very clear.\", \"subject score\": 1000, \"object score\": 802}}")
    ("subject" "UMLS:C0013227"))
   (("id"
     "NCBIGene:5594---SEMMEDDB:stimulates---None---None---increased---UMLS:C1417707---SEMMEDDB:")
    ("knowledge_source" "infores:semmeddb")
    ("object" "UMLS:C1417707")
    ("predicate" "biolink:regulates")
    ("publications" "PMID:30266829")
    ("publications_info"
     "{\"PMID:30266829\": {\"publication date\": \"2018 Oct 29\", \"sentence\": \"Notably, the role of ERK in the activation of Nfix is conserved postnatally in satellite cells, which represent the canonical myogenic stem cells of adult muscle.\", \"subject score\": 1000, \"object score\": 1000}}")
    ("subject" "NCBIGene:5594"))))
