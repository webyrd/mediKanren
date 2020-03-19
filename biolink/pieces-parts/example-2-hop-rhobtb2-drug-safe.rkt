(displayln "\nRunning 2-hop rhobtb2 query with drug safety constraint:")
(define q3 (time (query/graph
                  ((X       drug)
                   (Y       gene-or-protein)
                   (rhobtb2 "UMLS:C1425762")
                   (T       #f))
                  ((X->Y       negatively-regulates)
                   (Y->rhobtb2 positively-regulates)
                   ;; You can instead limit X to drugs with a tradename:
                   ;(X->T       '("has_tradename"))
                   ;; Just comment this line out if you choose to.
                   (X->T       drug-safe)
                   )
                  (X X->Y Y Y->rhobtb2 rhobtb2)
                  (X X->T T))))

(displayln "\nBuilding report:")
(pretty-print (time (report/query q3)))

#|
=>

((concepts: (X 11) (Y 1) (rhobtb2 1) (T 900))
 (edges: (X->Y 27) (Y->rhobtb2 2) (X->T 1019)))
|#

(displayln "\nRanking paths:")
(define ranked3 (time (ranked-paths q3)))
(pretty-ranked (take ranked3 1))

#|
=>

(path: 11 (X X->Y Y Y->rhobtb2 rhobtb2))
((0.925
  (("CUI:C0052416" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Arsenic trioxide" . "E2f transcription factor 1")
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
  (("CHEBI:41423" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("celecoxib" . "E2f transcription factor 1")
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
  (("CHEBI:66964" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("Delta(9)-tetrahydrocannabinol" . "E2f transcription factor 1")
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
  (("CHEBI:103210" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("hexahydrophthalic anhydride" . "E2f transcription factor 1")
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
  (("CHEBI:50924" . "CUI:C0812258") ("CUI:C0812258" . "CUI:C1425762"))
  (("sorafenib" . "E2f transcription factor 1")
   ("E2f transcription factor 1"
    .
    "Rho-related btb domain-containing protein 2"))))
|#
