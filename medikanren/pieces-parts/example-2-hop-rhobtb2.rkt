(displayln "\nRunning 2-hop rhobtb2 query with concept categories:")
(define q (time (query/graph
                  ((X       drug)
                   (Y       gene-or-protein)
                   (rhobtb2 "UMLS:C1425762"))
                  ((X->Y       negatively-regulates)
                   (Y->rhobtb2 positively-regulates))
                  (X X->Y Y Y->rhobtb2 rhobtb2))))

(displayln "\nBuilding report:")
(pretty-print (time (report/query q)))

#|
=>

((concepts: (X 52) (Y 1) (rhobtb2 1)) (edges: (X->Y 125) (Y->rhobtb2 2)))
|#

(displayln "\nRanking paths:")
(define ranked (time (ranked-paths q)))
(pretty-ranked ranked)

#|
=>

(path: 52 (X X->Y Y Y->rhobtb2 rhobtb2))
((0.925
  (("CUI:C0936225" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Inorganic arsenic" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0008546" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Chromatin" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0909265" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("pifithrin" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0669391" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Keratin-14 [Chemical/Ingredient]" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0052416" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Arsenic trioxide" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0243192" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("agonists" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C1435338" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("nutlin 3" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0005456" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Binding Sites" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0033259" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Prodigiosin" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0600508" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Response Elements" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CHEBI:4917" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("eugenol" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0048451" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("4-Methoxyamphetamine" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CHEBI:29073" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("L-ascorbic acid" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C1514727" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Ras Inhibitor" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0034424" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Quinolines [Chemical/Ingredient]" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C1443643" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Proteasome inhibitor" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CHEBI:41423" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("celecoxib" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C1099354" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("RNA, Small Interfering [Chemical/Ingredient]"
    .
    "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CHEBI:137113" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Jq1" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C1101610" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("MicroRNAs [Chemical/Ingredient]" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0012854" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Deoxyribonucleic acid" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0066084" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("methaneselenol" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0078375" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Alpha tocopherol succinate" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0069717" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Oxaliplatin" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CHEBI:7553" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Niclosamide" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0013227" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Pharmaceutical / biologic product" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CHEBI:66964" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Delta(9)-tetrahydrocannabinol" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CHEMBL:CHEMBL62381" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Sodium Butyrate" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CHEBI:3962" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("curcumin" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0003211" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Nonsteroidal Anti-inflammatory Drugs" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CHEBI:32243" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("tolfenamic acid" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CHEBI:40303" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("lovastatin" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CHEBI:63959" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("celastrol" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C1516312" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Caspase Inhibitor" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CHEBI:103210" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("hexahydrophthalic anhydride" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CUI:C0006632" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Cadmium" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.925
  (("CHEBI:49" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("(+)-Tetrandrine" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.85
  (("MESH:C000602704" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("UF010 compound" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.85
  (("MESH:C028031" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("cadmium acetate" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.85
  (("CHEBI:30621" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("diarsenic trioxide" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.7
  (("CHEBI:49603" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("lapatinib" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.7
  (("CHEBI:3181" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("bromocriptine" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.7
  (("CUI:C0950721" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("7-hydroxystaurosporine" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.7
  (("CHEBI:9516" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("thapsigargin" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.7
  (("CHEBI:50924" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("sorafenib" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.7
  (("CHEBI:223316" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("(+)-artemisinin" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.7
  (("CHEBI:338412" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("(-)-anisomycin" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.7
  (("CUI:C0084178" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Prohibitin" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.7
  (("CUI:C0010572" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Cycloheximide" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.7
  (("CUI:C0035668" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Ribonucleic acid" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.7
  (("CUI:C0961324" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("penetratin" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2")))
 (0.7
  (("CUI:C0173532" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Cyclin D3 [Chemical/Ingredient]" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2"))))
|#
