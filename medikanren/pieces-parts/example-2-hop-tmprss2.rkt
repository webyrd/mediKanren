(displayln "\nRunning 2-hop tmprss2 down-up query with concept categories:")
(define q1 (time (query/graph
                  ((X       drug)
                   (Y       gene-or-protein)
                   (tmprss2 "UMLS:C1336641"))
                  ((X->Y       negatively-regulates)
                   (Y->tmprss2 positively-regulates))
                  (X X->Y Y Y->tmprss2 tmprss2))))

(displayln "\nBuilding report:")
(pretty-print (time (report/query q1)))

#|
=>

((concepts: (X 701) (Y 7) (tmprss2 1)) (edges: (X->Y 1570) (Y->tmprss2 10)))
|#

(displayln "\nRanking paths:")
(define ranked (time (ranked-paths q1)))
(pretty-ranked ranked)

#|
=>

(path: 1010 (X X->Y Y Y->tmprss2 tmprss2))
((0.8340624999999999
  (("CHEBI:3347" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("candesartan" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.8340624999999999
  (("CHEBI:5959" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("irbesartan" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.818125
  (("CHEBI:7573" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("nilutamide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.818125
  (("CHEBI:27732" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("caffeine" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.818125
  (("CHEBI:91372" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("2-(4-morpholinyl)-6-(1-thianthrenyl)-4-pyranone" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.818125
  (("CHEBI:31932" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Olmesartan medoxomil" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C0135693" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Pd 123319" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CHEBI:6541" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("losartan" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C0006684" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Calcium channel blockers" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CHEBI:9241" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("spironolactone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C0009325" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Collagen" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C1257987" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Thiazolidinedione" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CHEBI:17609" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("cortisol 21-acetate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CHEBI:91739" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("2,2-diphenyl-N-[2,2,2-trichloro-1-[[(4-fluoro-3-nitroanilino)-sulfanylidenemethyl]amino]ethyl]acetamide"
    .
    "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C1123005" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("fulvestrant" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CHEBI:18357" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("(R)-noradrenaline" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C0521942" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Angiotensin II Receptor Antagonists" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C1320169" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Nonsteroidal antiandrogen" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CHEBI:9927" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("valsartan" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C0014939" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Estrogens" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C0002842" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Antiandrogens" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C0004504" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("Azole" . "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C0591237" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Casodex" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C0013227" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Pharmaceutical / biologic product"
    .
    "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C0003015" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Ace inhibitors" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C0285590" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Bicalutamide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C0012789" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Dithiothreitol [Chemical/Ingredient]" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CHEBI:41879" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("dexamethasone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CHEBI:17347" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("testosterone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CHEBI:17347" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("testosterone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C0022625" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Ketoconazole" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CHEMBL:CHEMBL491" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("HYDROXYFLUTAMIDE" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CHEBI:39548" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("atorvastatin" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CUI:C0001645" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Beta-adrenergic Blocking Agents" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.78625
  (("CHEBI:41879" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("dexamethasone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0117234" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Fadrozole [Chemical/Ingredient]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:8062" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("phenethyl caffeate" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0014930" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Antiestrogens/Modifiers" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0209366" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Cetrorelix" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0006938" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Captopril" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:46195" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("paracetamol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0242402" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Opioid receptor agonist" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:28119" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("2,3,7,8-tetrachlorodibenzodioxine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0257685" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Zoledronic acid" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0013227" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Pharmaceutical / biologic product" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0029984" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Oxadiazoles [Chemical/Ingredient]" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEMBL:CHEMBL1742441" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("EMBUSARTAN" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:15367" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("all-trans-retinoic acid" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0055226" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Cgs-16949a" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0443483" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Free testosterone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0032594" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Polysaccharide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:7466" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("nandrolone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:27584" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("aldosterone" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0065374" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Lisinopril" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0015684" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Fatty acid" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:31394" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Chlormadinone acetate" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0000975" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Acetate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:9168" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("rapamycin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0003308" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Antifungals" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16581" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("pregnenolone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0013227" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Pharmaceutical / biologic product" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0815016" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("releasing hormones" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0079925" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Antisense oligonucleotide" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0302837" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Vitamin supplementation" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0031705" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Phosphorus" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0065321" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Ly 53857" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0022245" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Isoproterenol" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0754775" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Kr 31080" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0243077" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("antagonists & inhibitors" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0667047" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("oleoyl-estrone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0044965" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("17 alpha-ethinyl-3-isopropylsulfonyloxyestradiol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0285590" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Bicalutamide" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:521033" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("dutasteride" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1318898" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("5-alpha Reductase Inhibitor [EPC]"
    .
    "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0014594" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Epitestosterone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:24536" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("hexachlorocyclohexane" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0028128" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Nitric oxide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0667225" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Krh 594" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:93368" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("2-(1-piperazinyl)quinoline" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:5132" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("flutamide" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0163305" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("pyrrolidine dithiocarbamic acid" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0600334" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Silibinin" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0023755" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Linuron" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:41774" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("tamoxifen" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEMBL:CHEMBL1355736" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Theophylline [Chemical/Ingredient]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:6715" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("medroxyprogesterone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEMBL:CHEMBL550495" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Methylene blue" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:5132" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("flutamide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0036800" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Serum Globulins [Chemical/Ingredient]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0301818" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Steroid hormone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:5120" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("fluoxymesterone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0040135" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Thyroid hormone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0017890" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Glycine" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:17045" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("dinitrogen oxide" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0291658" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Mk 996" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:3962" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("curcumin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1568245" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Endocrine Disruptors" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0175000" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("catalpol" . "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0054036" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Brefeldin A [Chemical/Ingredient]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0728964" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Ethinyl Estradiol/Norgestrel" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0593802" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Aromatase inhibitor" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1099354" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("RNA, Small Interfering [Chemical/Ingredient]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0008838" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Cisplatin" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:74125" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("losartan carboxylic acid" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0064113" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Itraconazole" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:4868" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("estramustine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0038785" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Sulfuric Acid Esters [Chemical/Ingredient]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0284825" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("6-anilino-5,8-quinolinedione" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:65329" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Ly294002" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:17609" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("cortisol 21-acetate" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0289313" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Rosiglitazone" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0309049" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("favor" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0647210" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("monooxyethylene trimethylolpropane tristearate" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0010505" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Cyanide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("GTPI:8041" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("KU-60019" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1564047" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Robenidine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0011038" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Dichlorodiphenyl Dichloroethylene [Chemical/Ingredient]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0000975" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Acetate" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEMBL:CHEMBL2140173" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("CHEMBL2140173" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0878449" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Metribolone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0042306" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Vanadium" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1254351" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Pharmacologic Substance" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:521033" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("dutasteride" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0079925" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Antisense oligonucleotide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16113" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("cholesterol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:76004" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("dimethyl fumarate" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0014939" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Estrogens" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:47808" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("(R)-sulforaphane" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16330" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("17beta-hydroxy-5alpha-androstan-3-one" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:24814" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("indole-3-methanol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:3766" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("clozapine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0025175" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Megestrol" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0072063" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("procymidone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0033497" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Propranolol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:28077" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("rifampicin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0667961" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Th 142177" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0209211" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Lanreotide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0591237" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Casodex" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0162772" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Reactive Oxygen Species [Chemical/Ingredient]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:22984" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("calcium atom" . "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0009906" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("oral contraceptive pill" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0022020" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Ionomycin" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16069" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("1H-imidazole" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0237795" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("pressors" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0000661" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("8-Bromo Cyclic Adenosine Monophosphate [Chemical/Ingredient]"
    .
    "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0043505" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Ziram product" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0055474" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("chlorpyrifos-methyl" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:49662" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("indomethacin" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:39421" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("perfluorooctane-1-sulfonic acid" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0220853" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Hydroxide Ion" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0299755" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("fonsartan" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1328699" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Ogx-011" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16422" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("androst-4-ene-3,17-dione" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0026814" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Muscarine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0006684" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Calcium channel blockers" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16356" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("3',5'-cyclic GMP" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0033607" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Peptide hydrolase inhibitor product" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0085885" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Lathyrus poison" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:47499" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("imipramine" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0078245" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("vinclozolin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0025685" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Methoxychlor" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:44185" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("methotrexate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0002555" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Aminoglutethimide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0017725" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Glucose" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:17026" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("progesterone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:34761" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("fenthion" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0770350" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("ovarian extract" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16973" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("11-deoxycorticosterone" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0003364" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Hypotensive agent" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:28088" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("genistein" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0049144" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("5-dihydro-19-nortestosterone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0033307" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Synthetic progestogen" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:50095" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("bucladesine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0012798" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Diuretics" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:77006" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("17beta-estradiol 3-benzoate" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1318898" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("5-alpha Reductase Inhibitor [EPC]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:5062" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("finasteride" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0567415" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Atom" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0247955" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("3-((2'-carboxybiphenyl-4-yl)methyl)-2-cyclopropyl-7-methyl-3H-imidazo(4,5-b)pyridine"
    .
    "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:41922" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("diethylstilbestrol" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0872893" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Estrogenic substances" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0527379" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Candesartan cilexetil" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0950793" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("osaterone acetate" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:17252" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("17alpha-hydroxyprogesterone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0243192" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("agonists" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0701303" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("mifepristone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0631746" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("pregnane-20-one" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0033671" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Protein Synthesis Inhibitors" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0007004" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Carbohydrate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0600434" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Peroxisome Proliferators" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:18145" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("(+)-alpha-tocopherol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0756836" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Ym 116" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0596922" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("methyl group" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:4911" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("etoposide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0360714" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("HMG-CoA reductase inhibitor product" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:63716" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("tenofovir hydrate" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0030817" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Penicillamine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0026160" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Mineralocorticoids" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1289975" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("androgenic steroid" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:37537" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("phorbol 13-acetate 12-myristate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1514555" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Protein Kinase C Inhibitor" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0017725" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Glucose" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0812407" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Glucuronide" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:131184" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("2-aminoethoxydiphenylborane" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0812407" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Glucuronide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:15948" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("lycopene" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0010572" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Cycloheximide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16469" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("17beta-estradiol" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0677666" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Soy protein isolate" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:59809" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("docetaxel trihydrate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:32260" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("trilostane" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:41922" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("diethylstilbestrol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0049205" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("5-hydroxy-2-N,N-dipropylaminotetralin"
    .
    "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:2704" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("anastrozole" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:3181" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("bromocriptine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:4903" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("17alpha-ethynylestradiol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:3757" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("clonidine (imino form)" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0002844" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Androgens" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:52717" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("bortezomib" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0172006" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Ono 3805" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:88200" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("benazeprilat" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0090052" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("11-hydroxytestosterone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0043031" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Warfarin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0521966" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Calcium ionophore A23187" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:138000" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("3-(1-methylpyrrolidin-2-yl)pyridine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:119486" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("efavirenz" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:28088" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("genistein" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0291375" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("4,7beta-dimethyl-4-azacholestan-3-one"
    .
    "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0027415" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("narcotic" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0070915" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("phosphoramidon" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0019932" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Hormone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0012854" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Deoxyribonucleic acid" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0664210" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("7-hydroxytestosterone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0090050" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("11-beta Hydroxyandrostenedione" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0286256" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Cgp 42112a" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:5692" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("hexachlorobenzene" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0038317" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Steroid" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16881" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("1H-indole" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16243" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("quercetin" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0003765" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Arginine" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0109641" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("2,2,5,7,8-pentamethyl-1-hydroxychroman" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0035339" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Retinoids" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0297076" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("4-(3-(3-(bis(4-isobutylphenyl)methylamino)benzoyl)-1H-indol-1-yl)butyric acid"
    .
    "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0052201" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Apolipoprotein E4" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0059497" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Equol [Chemical/Ingredient]" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0001645" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Beta-adrenergic Blocking Agents" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:88522" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Monobutylphthalate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0020393" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Hydroxysteroids [Chemical/Ingredient]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:41879" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("dexamethasone"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0023755" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Linuron" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:8364" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("prazosin" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0005456" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Binding Sites" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16962" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("cortisone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0600334" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Silibinin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0950211" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("oxendolone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1100708" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("3,3-diindolymethane" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1611640" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("therapeutic agent" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0012854" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Deoxyribonucleic acid" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0603200" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("1,2-di-(4-sulfamidophenyl)-4-butylpyrazolidine-3,5-dione" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0768712" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("8-prenylnaringenin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0120107" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Goserelin product" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0003308" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("Antifungals" . "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0392419" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Fungicide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:27584" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("aldosterone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0032458" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Polycyclic Aromatic Hydrocarbons [Chemical/Ingredient]"
    .
    "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:50742" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("cyproterone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0577749" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Phytochemical product" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:17026" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("progesterone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0728965" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Ethinyl Estradiol-Norgestrel Combination" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0028128" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Nitric oxide" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:3757" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("clonidine (imino form)" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0014594" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Epitestosterone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:28748" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("doxorubicin" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0010124" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Corticosterone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:8107" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("phenytoin" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:50729" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("mitoxantrone" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:28940" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("calciol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0036884" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Sex hormone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:7590" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("nitrofen" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0657318" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("lysyl-arginyl-alanyl-lysyl-alanyl-lysyl-threonyl-threonyl-lysyl-lysyl-arginine"
    .
    "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0024467" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Magnesium" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0017725" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Glucose" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1101610" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("MicroRNAs [Chemical/Ingredient]" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0257337" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("17 beta-benzoyl-4-aza-5 alpha-androst-1-ene-3-one"
    .
    "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0013832" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Electrolytes" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0002845" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Anabolic Steroids" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0136082" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("perchlorate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:3647" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("chlorpromazine" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEMBL:CHEMBL187460" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("CRYPTOTANSHINONE" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0289313" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Rosiglitazone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:4532" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("diethylstilbestrol diphosphate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:8434" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("prochloraz" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0700620" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Losartan" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:5775" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("hydralazine" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:17883" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("hydrogen chloride" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16422" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("androst-4-ene-3,17-dione" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:18220" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("isoflavone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0304497" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Cytotoxic" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16503" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("selane" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0243076" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("antagonists & inhibitors" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:27543" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("9H-carbazole" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0004147" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Atenolol" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0486333" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Androstanediol" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0032447" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Polychlorinated biphenyl" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:28820" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("9-phenanthrol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:31189" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Allylestrenol" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:41774" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("tamoxifen" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:45713" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("trans-resveratrol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0053526" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Bethanechol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0252643" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Bosentan" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0772394" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Lepirudin" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0034085" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Lung surfactant" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEMBL:CHEMBL170630" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("ETHANE" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0301818" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Steroid hormone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0075116" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("Src peptide" . "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:6715" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("medroxyprogesterone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0073667" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("11,17-dihydroxy-6-methyl-17-(1-propynyl)androsta-1,4,6-triene-3-one"
    .
    "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0010124" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("Corticosterone"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0878412" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Calcimycin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1514972" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Steroidal Estrogen" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:28324" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("11-deoxycortisol" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:2666" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("amitriptyline" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0057277" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Dehydroepiandrosterone sulfate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:39867" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("valproic acid" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:5001" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("fenofibrate" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:58987" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("sildenafil citrate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0083966" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Pd 123177" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0149473" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Zoladex" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0076180" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("testosterone 17 beta-carboxylic acid"
    .
    "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0163951" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("androstan-3-one" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0006456" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Buserelin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:26836" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("sulfuric acid" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:15367" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("all-trans-retinoic acid" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0071649" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Polyphenol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0040615" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Antipsychotics" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:8707" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("quetiapine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0043481" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Zinc" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:28368" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("novobiocin" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:15379" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("oxygen" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0070455" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Permethrin" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0351241" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Androgens+anabolic steroids" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0127279" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Plomestane" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0009148" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Cobalt" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0028910" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Oils,essential" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0037343" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Autacoids [Chemical/Ingredient]" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0003765" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Arginine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:3699" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("cimetidine" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0035696" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Messenger RNA" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0034595" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Radioactive Isotopes" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1257954" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Cox-2 inhibitor" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:31186" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("alfacalcidol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:34849" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("mibolerone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0213165" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Fr 139317" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:15367" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("all-trans-retinoic acid" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:45951" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("trifluoperazine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0038317" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Steroid" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0119192" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("ginsenoside Rb1" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0243192" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("agonists" . "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1099354" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("RNA, Small Interfering [Chemical/Ingredient]" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0082608" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Fluvastatin" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:27974" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("estriol" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0043791" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Diglyceride" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0071642" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("polypeptide C" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0360528" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Dexamethasone in oral dosage form" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1337263" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("hamster protein" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0028005" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Nicardipine" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:17747" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("bis(2-ethylhexyl) phthalate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEMBL.COMPOUND:CHEMBL834" . "CUI:C0002844")
   ("CUI:C0002844" . "CUI:C1336641"))
  (("PAMIDRONIC ACID" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:6443" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("levonorgestrel" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:17263" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("estrone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:31822" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("mercaptopurine hydrate" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0243042" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Mediator of inflammation" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:9434" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("telmisartan" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0026234" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Plicamycin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:22984" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("calcium atom" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0030956" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Peptide product" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:35297" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("acene" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:135735" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("delapril" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0002842" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Antiandrogens" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0673767" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Kr 31080" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0032458" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Polycyclic Aromatic Hydrocarbons [Chemical/Ingredient]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:18243" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("dopamine" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0000979" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Acetates [Chemical/Ingredient]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0174494" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("candesartan cilexetil" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEMBL:CHEMBL1977579" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Streptozocin" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0017945" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Glycol" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1443643" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Proteasome inhibitor" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0009871" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Contraceptives" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0014025" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Enalapril" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0772453" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Grape seed" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:68642" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("abiraterone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("GTPI:9359" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("CP466722" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:8378" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("prednisolone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0007220" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Cardiovascular medications" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0010592" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("Cyclosporine" . "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0619285" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("1,4-dioxin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("GTPI:8003" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("VE-822" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1258800" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Anabolic Agents" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:40303" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("lovastatin" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0242893" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Cholinergic Agents" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:584020" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("methysergide" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0114873" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Doxazosin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0242947" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Muscarinic receptor agonist product" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0071554" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Polyestradiol phosphate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0010124" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Corticosterone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:4910" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("etomidate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0009905" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("oral contraceptive" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1446539" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Tegafur and uracil product" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0030956" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Peptide product" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0004471" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Aza Compounds [Chemical/Ingredient]"
    .
    "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:17252" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("17alpha-hydroxyprogesterone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0178528" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("carboxylate" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:15738" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("staurosporine" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0169911" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("messenger protein"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0020616" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Hypoglycemic agent" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0376202" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Pregnanolone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0304402" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Stimulant" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0001617" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Adrenal corticosteriods" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0038836" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Superoxides [Chemical/Ingredient]" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:30621" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("diarsenic trioxide" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0360714" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("HMG-CoA reductase inhibitor product" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0879272" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("bortezomib" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0005050" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Benzimidazoles [Chemical/Ingredient]" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1257987" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Thiazolidinedione" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0017774" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Glucuronates [Chemical/Ingredient]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0815278" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Neurotransmitter Agents" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0025865" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Metribolone [Chemical/Ingredient]"
    .
    "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:45906" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("suramin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:34873" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("N-nitrosodiethylamine" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0174112" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("(5alpha)-23-methyl-4-aza-21-norchol-1-ene-3,20-dione"
    .
    "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0245514" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Troglitazone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0044981" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("17-N,N-diethylcarbamoyl-4-methyl-4-azaandrostane-3-one"
    .
    "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0035696" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Messenger RNA" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0356799" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Estramustine phosphate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:15551" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("prostaglandin E2" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0074926" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Soy protein" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:103210" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("hexahydrophthalic anhydride"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:22984" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("calcium atom" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:31521" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("doxifluridine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0074721" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Sodium azide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0014938" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Estrogens,conjugated" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0699161" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("norethindrone acetate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:45081" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("pentamidine" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0003402" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("antioxidant" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0017710" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Glucocorticoids" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0162772" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Reactive Oxygen Species [Chemical/Ingredient]" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0012798" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Diuretics" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16469" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("17beta-estradiol" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0006055" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Clostridium botulinum toxin" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0010572" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Cycloheximide" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0038734" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Sulfhydryls" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16118" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("berberine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:50885" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("fludrocortisone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0662900" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Amd3100" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0124804" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("L 158809" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0064294" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Keratinocyte growth factor" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:52289" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("wortmannin" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:164200" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("triclosan" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0243192" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("agonists" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0013162" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Drug Combinations" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0003392" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Antineoplastics" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:16469" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("17beta-estradiol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:103210" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("hexahydrophthalic anhydride" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0878449" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Metribolone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:31394" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Chlormadinone acetate" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:28689" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("dehydroepiandrosterone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0018282" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Growth Inhibitors" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1312096" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("15-deoxyprostaglandin J2" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0037239" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("2,3,4,5-Tetrahydro-7,8-dihydroxy-1-phenyl-1H-3-benzazepine" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:4814" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("eprosartan" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0243192" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("agonists" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0013879" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Chemical element" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEMBL:CHEMBL1697830" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Chlormadinone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:39867" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("valproic acid" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:28925" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("mechlorethamine" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:45924" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("trimethoprim" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:8382" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("prednisone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:27617" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("monensin A" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0027289" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Nadh" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0720298" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Estrogenic" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0066718" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("mono-(2-ethylhexyl)phthalate" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:5062" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("finasteride" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:7930" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Pargyline" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0067762" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("n-Acetyl neuraminic acid" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:47898" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("4'-epidoxorubicin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1565238" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("2-morpholin-4-yl-6-thianthren-1-yl-pyran-4-one" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:32020" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("pitavastatin" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0025219" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Melatonin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:37941" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("clopidogrel" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:7553" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Niclosamide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0017945" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Glycol" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0002605" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Benactyzine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0755562" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("U-0126" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0593802" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Aromatase inhibitor" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:7735" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("olanzapine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:31552" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Epristeride" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:17026" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("progesterone" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0253596" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Activating Transcription Factor 3 [Chemical/Ingredient]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0033306" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Progestins" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEMBL:CHEMBL491" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("HYDROXYFLUTAMIDE" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:6801" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("metformin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:80025" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("Salinomycin" . "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:50742" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("cyproterone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C1312096" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("15-deoxyprostaglandin J2" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0243076" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("antagonists & inhibitors" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0037135" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Silymarin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0071097" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Pioglitazone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0024547" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Malathion" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:31536" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("emtricitabine" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:17263" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("estrone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0379142" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("cyclo(Trp-Asp-Pro-Val-Leu)" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0022640" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Ketosteroids [Chemical/Ingredient]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CHEBI:45906" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("suramin" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.7224999999999999
  (("CUI:C0162758" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Serotonin Uptake Inhibitors" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0599786" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("pollutant" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:45783" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("imatinib" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0038317" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Steroid" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0062565" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Herbimycin A" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:16113" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("cholesterol" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C1171350" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Kinase Inhibitor [EPC]" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0051834" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Androsterone Glucuronide" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:9927" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("valsartan" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0010572" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Cycloheximide" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0049647" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("6-methyl-1,3,8-trichlorodibenzofuran"
    .
    "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:16898" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("pyrimidine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0521942" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Angiotensin II Receptor Antagonists" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0014417" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Pollutant" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0025219" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Melatonin" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:3347" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("candesartan" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0005372" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Bicuculline [Chemical/Ingredient]" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0246173" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("trestolone" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:7627" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("norethisterone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0035668" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("Ribonucleic acid"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:17939" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("puromycin" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0033497" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("Propranolol" . "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:71418" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("triamcinolone acetonide" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0036847" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("Sesquiterpenes [Chemical/Ingredient]"
    .
    "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:8273" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("plumbagin" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:16240" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("hydrogen peroxide"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0030493" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("Paraquat" . "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:49668" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("gefitinib" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:5959" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("irbesartan" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:46081" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("fluconazole" . "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C1602789" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Profen" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0135693" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Pd 123319" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C1171350" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Kinase Inhibitor [EPC]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:39548" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("atorvastatin" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:15738" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("staurosporine" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0002696" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Ampyrone [Chemical/Ingredient]" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0012935" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Deoxyribonucleic acid, single stranded" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0768581" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("4-ethoxymethylene-2-phenyl-2-oxazoline-5-one" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:31932" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Olmesartan medoxomil" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0599683" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("C19 steroid" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:5613" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("haloperidol" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0003448" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Antituberculars" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEMBL:CHEMBL1908314" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Mespirenone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0378018" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("astragaloside A" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C1268567" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("Protein-tyrosine kinase inhibitor product"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0077274" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Triptolide" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0619285" . "CHEBI:16330") ("CHEBI:16330" . "CUI:C1336641"))
  (("1,4-dioxin" . "17beta-hydroxy-5alpha-androstan-3-one")
   ("17beta-hydroxy-5alpha-androstan-3-one"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0043454" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Zearalenone" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0599740" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("pharmacophore" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C1567637" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("Rpr260243" . "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0040615" . "CUI:C0599295") ("CUI:C0599295" . "CUI:C1336641"))
  (("Antipsychotics"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")
   ("V-ets avian erythroblastosis virus e26 oncogene homolog"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C1377705" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("preservative free" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C1510932" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("antiprogestin" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0012789" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Dithiothreitol [Chemical/Ingredient]" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0917964" . "CUI:C0919524") ("CUI:C0919524" . "CUI:C1336641"))
  (("Forskolin" . "Atm")
   ("Atm" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0068957" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("noramidopyrine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:32169" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Sulpyrine" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CHEBI:18357" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("(R)-noradrenaline" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0002845" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Anabolic Steroids" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.595
  (("CUI:C0035668" . "CUI:C0002844") ("CUI:C0002844" . "CUI:C1336641"))
  (("Ribonucleic acid" . "Androgens")
   ("Androgens" . "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0870883" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Metabolite" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:63867" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("N-(hexanoyl)sphing-4-enine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0950721" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("7-hydroxystaurosporine" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0527855" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("angelmicin B" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0673767" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Kr 31080" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:5292" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("geldanamycin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1137199" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("atractylenolide II" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0717758" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Etanercept" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0205838" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Lanthanoid Series Elements [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0009968" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Copper" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0949647" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Tocotrienol" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:45713" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("trans-resveratrol" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0526226" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("garcinol" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1144513" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("mahanine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:34967" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Sr 141716" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:17609" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("cortisol 21-acetate" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:43616" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("K-252a" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:46024" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("trichostatin A" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0001218" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Acrylamides [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0013162" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Drug Combinations" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0005029" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Benzamides [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:22984" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("calcium atom" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0020737" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Ibogaine" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0243192" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("agonists" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:15367" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("all-trans-retinoic acid" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:65329" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Ly294002" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0210353" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("N-(2(R)-2-(hydroxamidocarbonylmethyl)-4-methylpentanoyl)-L-tryptophan methylamide"
    .
    "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0038760" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Sulfonamides" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:32020" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("pitavastatin" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0065321" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Ly 53857" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0381241" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Tyrphostin B42" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0017725" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Glucose" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0007090" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Carcinogen" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:17026" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("progesterone" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0043775" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("1,2-bis(2-aminophenoxy)ethane-N,N,N',N'-tetraacetic acid"
    .
    "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0034861" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Recombinant protein" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0282555" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Anti-Allergic Agents" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0054836" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Carvedilol" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1514560" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Protein Phosphatase Inhibitor" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0302837" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Vitamin supplementation" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0001617" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Adrenal corticosteriods" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:6402" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("leflunomide" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0013227" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Pharmaceutical / biologic product" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0012854" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Deoxyribonucleic acid" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:28364" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("all-cis-5,8,11,14,17-icosapentaenoic acid" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0536128" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Xanthohumol" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0079925" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Antisense oligonucleotide" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:44215" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("NAD zwitterion" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1099354" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("RNA, Small Interfering [Chemical/Ingredient]" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:27584" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("aldosterone" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0244104" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Pyruvate" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:16240" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("hydrogen peroxide" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1565237" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("2-morpholin-4-yl-6-thianthren-1-yl-pyran-4-one" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:50924" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("sorafenib" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1328709" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("siplizumab" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0060397" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("fisetin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0667961" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Th 142177" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0603200" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("1,2-di-(4-sulfamidophenyl)-4-butylpyrazolidine-3,5-dione"
    .
    "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0071642" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("polypeptide C" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEMBL:CHEMBL92484" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Aptiganel" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0002520" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Amino acids" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0028005" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Nicardipine" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0599740" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("pharmacophore" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1134434" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Ag 1879" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:17045" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("dinitrogen oxide" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1176007" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Piceatannol" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0030493" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Paraquat" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0971797" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("halenaquinol" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0051936" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("ansamycin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0600434" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Peroxisome Proliferators" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1566074" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("salubrinal" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:30740" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("ethylene glycol bis(2-aminoethyl)tetraacetic acid" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0243076" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("antagonists & inhibitors" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:28088" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("genistein" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:4948" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Evodiamine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:45863" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("paclitaxel" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:16933" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("ergosterol" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:15367" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("all-trans-retinoic acid" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0002007" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Aldosterone receptor antagonist" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:40303" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("lovastatin" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEMBL:CHEMBL1742441" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("EMBUSARTAN" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0010592" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Cyclosporine" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0174494" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("candesartan cilexetil" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0028128" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Nitric oxide" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:5292" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("geldanamycin" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:17097" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("biphenyl" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0028910" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Oils,essential" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0017890" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Glycine" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0054130" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("bromopyruvate" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:89642" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Gestrinone" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:8364" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("prazosin" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:88200" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("benazeprilat" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0037907" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Sphingosine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0070913" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Phosphoramide Mustard" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:9908" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("ursolic acid" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0012968" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Docosahexaenoic Acids [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:63616" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("pemetrexed" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0443985" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Flinders medical center-7 marker" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0029904" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Ouabain" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0298346" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("MEK Inhibitor PD-98059" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0718263" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Adenosine-5" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0162772" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Reactive Oxygen Species [Chemical/Ingredient]" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0043791" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Diglyceride" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:114785" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("erlotinib" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:50729" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("mitoxantrone" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0301067" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Cyclohexamide" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:103210" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("hexahydrophthalic anhydride" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0000641" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("7,8-Dihydro-7,8-dihydroxybenzo(a)pyrene 9,10-oxide [Chemical/Ingredient]"
    .
    "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0069535" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Oncostatin M [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0033497" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Propranolol" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0754775" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Kr 31080" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:8890" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("(S)-ropivacaine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1099354" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("RNA, Small Interfering [Chemical/Ingredient]" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0047611" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("3-methylquercetin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:28088" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("genistein" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:103210" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("hexahydrophthalic anhydride" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:4806" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("epigallocatechin gallate" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0034407" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Quinazoline" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0243042" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Mediator of inflammation" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0003402" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("antioxidant" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0078433" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Nerve agent VX" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0005372" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Bicuculline [Chemical/Ingredient]" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0148973" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("xestoquinone" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0040615" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Antipsychotics" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:45924" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("trimethoprim" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0911464" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Pd 173955" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0012802" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Thiazides/related diuretics" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:63716" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("tenofovir hydrate" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0914052" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Tyrphostin AG 1295" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0014994" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Ether" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:52717" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("bortezomib" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:3757" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("clonidine (imino form)" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0299755" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("fonsartan" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:5123" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("fluphenazine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:16973" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("11-deoxycorticosterone" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:5613" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("haloperidol" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1611640" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("therapeutic agent" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1527392" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Immunomodulators" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:16881" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("1H-indole" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1259477" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Ym-254890" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:15553" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("prostaglandin F2alpha" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0667225" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Krh 594" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0718711" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Atacand" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0069796" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("oxotremorine M" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:5775" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("hydralazine" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:45951" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("trifluoperazine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0041485" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Tyrosine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0032136" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Plasmid" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0026234" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Plicamycin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0003015" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Ace inhibitors" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1443775" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Epidermal growth factor receptor antagonist product" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0030956" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Peptide product" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0014025" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Enalapril" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0213654" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Tyrphostin A25" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0124804" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("L 158809" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0013162" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Drug Combinations" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1101610" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("MicroRNAs [Chemical/Ingredient]" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0017725" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Glucose" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0010124" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Corticosterone" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:3962" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("curcumin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0027289" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Nadh" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0567415" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Atom" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0386394" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Olmesartan Medoxomil" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0080124" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("antisense-RNA" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:9448" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("terbinafine" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0047701" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("3-phosphoglycerate" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0006938" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Captopril" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0017710" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Glucocorticoids" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:28748" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("doxorubicin" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:50924" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("sorafenib" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0034325" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Pyrrole" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0038734" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Sulfhydryls" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:8899" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("rottlerin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:175901" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("gemcitabine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0230664" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Membrane receptor" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0003765" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Arginine" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:37537" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("phorbol 13-acetate 12-myristate" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0042523" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Verapamil" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:37941" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("clopidogrel" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1312096" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("15-deoxyprostaglandin J2" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0036257" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Dibenz(b,f)(1,4)oxazepine-10(11H)-carboxylic acid, 8-chloro-, 2-acetylhydrazide"
    .
    "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0003392" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Antineoplastics" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:46345" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("5-fluorouracil" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:4814" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("eprosartan" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0769339" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Kf-25706" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0008466" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Chondroitin sulfate" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0124673" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Kn 93" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:18243" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("dopamine" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1328819" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Absolute Genomic Position" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0597298" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Protein Isoforms [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0909826" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Claudin-4 [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0038836" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Superoxides [Chemical/Ingredient]" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0163305" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("pyrrolidine dithiocarbamic acid" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0002508" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Amine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:65408" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("andrographolide" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:3962" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("curcumin" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:6541" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("losartan" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:39112" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("bosutinib" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0003308" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Antifungals" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0030935" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Peptide Fragments [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0004504" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Azole" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:43572" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("kojic acid" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0178528" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("carboxylate" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0243192" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("agonists" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0018282" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Growth Inhibitors" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0935987" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Gleevec" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0033607" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Peptide hydrolase inhibitor product" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1097641" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("2-cyclopentyl-5-(5-isoquinolylsulfonyl)-6-nitro-1H-benzo(D)imidazole"
    .
    "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0360714" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("HMG-CoA reductase inhibitor product" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0732611" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Selective Estrogen Receptor Modifying Agents" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0034435" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Quinones [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:16240" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("hydrogen peroxide" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0647210" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("monooxyethylene trimethylolpropane tristearate" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0003235" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Antifungal Agents" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1137338" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("4-(5-benzo(1,3)dioxol-5-yl-4-pyridin-2-yl-1H-imidazol-2-yl)benzamide"
    .
    "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0596087" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Antiangiogenic Agents" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0012798" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Diuretics" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0768581" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("4-ethoxymethylene-2-phenyl-2-oxazoline-5-one" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:135737" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("lacidipine" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:27617" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("monensin A" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0020282" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Hydrogen sulfide" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0009325" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Collagen" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0521966" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Calcium ionophore A23187" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0755562" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("U-0126" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0029984" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Oxadiazoles [Chemical/Ingredient]" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:17241" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("1H-pyrazole" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEMBL:CHEMBL475584" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("1-(4-(4-amino-5-(3-methoxyphenyl)-7H-pyrrolo[2,3-d]pyrimidin-7-yl)phenethyl)piperidin-4-ol"
    .
    "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0021966" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Iodide salt" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0618845" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("N-formyl-13-dihydrocarminomycin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:135735" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("delapril" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0023779" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Lipid" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEMBL:CHEMBL62381" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Sodium Butyrate" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1377705" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("preservative free" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0008864" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Citrulline" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0009325" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Collagen" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0022245" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Isoproterenol" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:16227" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("pyridine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:6801" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("metformin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1572759" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Monohydrate" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:17362" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("quinoline" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0297666" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Sb 203580" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0256547" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("anacardic acid" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0025405" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Tiopronin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0657318" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("lysyl-arginyl-alanyl-lysyl-alanyl-lysyl-threonyl-threonyl-lysyl-lysyl-arginine"
    .
    "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0034136" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Purine nucleoside product" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:15864" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("luteolin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0246694" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Mg-132" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0008240" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Chlorogenic acid" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0247955" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("3-((2'-carboxybiphenyl-4-yl)methyl)-2-cyclopropyl-7-methyl-3H-imidazo(4,5-b)pyridine"
    .
    "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0003316" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Epitope" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1443643" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Proteasome inhibitor" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0252643" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Bosentan" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0567415" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Atom" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:16469" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("17beta-estradiol" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0079925" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Antisense oligonucleotide" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0031453" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Phenylalanine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:9241" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("spironolactone" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0213165" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Fr 139317" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:9434" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("telmisartan" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1567637" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Rpr260243" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0175000" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("catalpol" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0286256" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Cgp 42112a" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0879551" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Visilizumab" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:76004" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("dimethyl fumarate" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0037874" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Spermine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0069035" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("norvaline" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:41879" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("dexamethasone" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0178528" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("carboxylate" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1097828" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("denbinobin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0597128" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("ocular hypotensive" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0010738" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Cytochalasin D [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0001047" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Acetylcysteine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0391142" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("N-beta-alanyl-5-S-glutathionyl-3,4-dihydroxyphenylalanine"
    .
    "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0022020" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Ionomycin" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1101610" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("MicroRNAs [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:30563" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("silicon dioxide" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1328819" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Absolute Genomic Position" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0035696" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Messenger RNA" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0878412" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Calcimycin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1514555" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Protein Kinase C Inhibitor" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0813872" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Dizocilpine Maleate" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0005456" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Binding Sites" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0082608" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Fluvastatin" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0008015" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Macrophage chemotactic factor" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0700620" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Losartan" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0080124" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("antisense-RNA" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0058079" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("dihydroguaiaretic acid" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0043791" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Diglyceride" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:119486" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("efavirenz" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:18388" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("apigenin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0027388" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Naphthoquinone" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1565238" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("2-morpholin-4-yl-6-thianthren-1-yl-pyran-4-one" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:556075" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("radicicol" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0006521" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Butyrates [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0967397" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("halenaquinone" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEMBL:CHEMBL7463" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("GF-109203" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:17303" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("morphine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0004147" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Atenolol" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0280038" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Purine antagonist" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:27584" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("aldosterone" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0013832" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Electrolytes" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0083966" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Pd 123177" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1564139" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Mutant Proteins [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:16118" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("berberine" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:31932" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Olmesartan medoxomil" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:46081" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("fluconazole" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0027415" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("narcotic" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1608746" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("4-quinazolinamine, N-(2-chloro-5-methoxyphenyl)-6-methoxy-7-((1-methyl-4-piperidinyl) methoxy)"
    .
    "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0040880" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Triazole" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0010737" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Cytochalasin B [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0619285" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("1,4-dioxin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0005456" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Binding Sites" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1257987" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Thiazolidinedione" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0000254" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("2-Amino-5-phosphonovalerate [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1120337" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("tyrphostin AG 1024" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0379142" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("cyclo(Trp-Asp-Pro-Val-Leu)" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0007412" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Catecholamine product" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0075116" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Src peptide" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:49960" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("vandetanib" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0037239" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("2,3,4,5-Tetrahydro-7,8-dihydroxy-1-phenyl-1H-3-benzazepine"
    .
    "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0003402" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("antioxidant" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:28364" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("all-cis-5,8,11,14,17-icosapentaenoic acid" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0729218" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("tacrolimus" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:17793" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("calycosin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:5001" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("fenofibrate" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0034140" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Purines [Chemical/Ingredient]" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0066801" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("morin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1519952" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Vanilloid" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0283149" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("ophiocordin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0169911" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("messenger protein" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0010803" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Cytokeratin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:90695" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("anthra[1,9-cd]pyrazol-6(2H)-one" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:28901" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("busulfan" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1095795" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("EGb761" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0298346" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("MEK Inhibitor PD-98059" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1277078" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Human red blood cells, blood product" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0168957" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("halenaquinol sulfate" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0032172" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Platelet activating factor" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:22984" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("calcium atom" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0005456" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Binding Sites" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0025646" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Methionine" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:566274" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("malonaldehyde" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0053065" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Beauvericin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0035696" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Messenger RNA" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0165117" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("jasplakinolide" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:5601" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("(+)-haematoxylin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0962522" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Su 6656" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0005050" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Benzimidazoles [Chemical/Ingredient]" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0218197" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("cardiotoxin III, Naja naja atra" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0075116" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Src peptide" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:17939" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("puromycin" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:9168" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("rapamycin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:15551" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("prostaglandin E2" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:6456" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("lidocaine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0037343" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Autacoids [Chemical/Ingredient]" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0647210" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("monooxyethylene trimethylolpropane tristearate" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:39867" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("valproic acid" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:28088" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("genistein" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:15422" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Atp" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0034424" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Quinolines [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0001771" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Agar" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:16356" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("3',5'-cyclic GMP" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0597220" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("phospholipase inhibitor" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEMBL:CHEMBL1977579" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Streptozocin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:131184" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("2-aminoethoxydiphenylborane" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:9287" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("streptonigrin" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0021528" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Inosine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0378018" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("astragaloside A" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1449702" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Protein kinase inhibitor" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0030956" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Peptide product" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0008838" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Cisplatin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0008838" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Cisplatin" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:49603" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("lapatinib" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:74125" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("losartan carboxylic acid" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0038836" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Superoxides [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0070610" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("phenyl-2-aminoethyl sulfide" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0906802" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("imatinib mesylate" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1099354" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("RNA, Small Interfering [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0003364" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Hypotensive agent" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0162772" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Reactive Oxygen Species [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0527379" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Candesartan cilexetil" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0765885" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("beta-hydroxyisovalerylshikonin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0010934" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Dactinomycin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0597217" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("phosphatase inhibitor" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:16113" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("cholesterol" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:6570" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("lupeol" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0289313" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Rosiglitazone" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0961045" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Ekb-569" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0682957" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("amine derivatives" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0964758" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Ws 1442" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0971473" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("cilengitide" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0065374" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Lisinopril" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:38912" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Ro 31-8220" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:27732" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("caffeine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0063340" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Ifenprodil" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0070915" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("phosphoramidon" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:65329" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Ly294002" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0062565" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Herbimycin A" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:9516" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("thapsigargin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:80025" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Salinomycin" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0237795" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("pressors" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0006684" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Calcium channel blockers" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0029366" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Vanadates" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:8062" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("phenethyl caffeate" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1566558" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("natural product" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:31536" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("emtricitabine" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0013879" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Chemical element" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0042305" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Vanadates [Chemical/Ingredient]" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0045246" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("2,2'-azino-di-(3-ethylbenzothiazoline)-6-sulfonic acid" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:59809" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("docetaxel trihydrate" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0175000" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("catalpol" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0038317" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Steroid" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:47495" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("N-[2-(4-bromocinnamylamino)ethyl]isoquinoline-5-sulfonamide"
    .
    "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0001645" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Beta-adrenergic Blocking Agents" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0772394" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Lepirudin" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:27732" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("caffeine" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:46024" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("trichostatin A" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0914738" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("benzocycloheptapyridine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0012544" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Bisphosphonate" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0033497" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("Propranolol" . "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:8774" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("ramipril" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C1171350" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Kinase Inhibitor [EPC]" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:63618" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("pravastatin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0699251" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("acetylcysteine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0042010" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Uridine triphosphate" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0020349" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Hydroxyeicosatetraenoic acid" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:52289" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("wortmannin" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:17154" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("nicotinamide" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:92413" . "CUI:C1705964") ("CUI:C1705964" . "CUI:C1336641"))
  (("5-[(2,5-dihydroxyphenyl)methyl-[(2-hydroxyphenyl)methyl]amino]-2-hydroxybenzoic acid"
    .
    "ERG wt Allele")
   ("ERG wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0291658" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("Mk 996" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0048451" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("4-Methoxyamphetamine" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0040270" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("1,2-Dihydroxybenzene-3,5-Disulfonic Acid Disodium Salt [Chemical/Ingredient]"
    .
    "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0013227" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Pharmaceutical / biologic product" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0014939" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Estrogens" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:15843" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("arachidonic acid" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CUI:C0042523" . "CUI:C1705997") ("CUI:C1705997" . "CUI:C1336641"))
  (("Verapamil" . "SRC wt Allele")
   ("SRC wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog")))
 (0.48999999999999994
  (("CHEBI:52289" . "CUI:C1705846") ("CUI:C1705846" . "CUI:C1336641"))
  (("wortmannin" . "ATM wt Allele")
   ("ATM wt Allele"
    .
    "V-ets avian erythroblastosis virus e26 oncogene homolog"))))
|#
