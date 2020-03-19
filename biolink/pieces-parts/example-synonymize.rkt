;; Find CURIE synonyms (and their descriptive names, for convenience) for the
;; given CURIE.


;; celecoxib
(curie-synonyms/names "CHEBI:41423")
;; This expression will give the same result because it's a synonym.
;; The same should hold for all CURIE synonyms.
;(curie-synonyms/names "UMLS:C0538927")

#|
=>

(("MTHSPL:JCX84Q7J1L" . "Celecoxib")
 ("CHEMBL.COMPOUND:CHEMBL118" . "CELECOXIB")
 ("DrugBank:DB00482" . "Celecoxib")
 ("CUI:C0538927" . "Celecoxib")
 ("MeSH:C105934" . "Celecoxib")
 ("CHEMBL:CHEMBL118" . "Celecoxib")
 ("NDFRT:N0000007233" . "Celecoxib [Chemical/Ingredient]")
 ("HMDB:HMDB0005014" . "Celecoxib")
 ("DRUGBANK:DB00482" . "Celecoxib")
 ("CHEBI:41423" . "celecoxib")
 ("RXNORM:140587" . "celecoxib")
 ("PUBCHEM:2662" . "Celecoxib")
 ("NDFRT:N0000148596" . "Celecoxib")
 ("UMLS:C0538927" . "celecoxib"))
|#


;; e2f1
(curie-synonyms/names "UMLS:C0812258")

#|
=>

(("UMLS:C0812258" . "E2F1 gene")
 ("OMIM:189971" . "E2f transcription factor 1")
 ("NCI:C18379" . "E2F1 gene")
 ("UniProtKB:Q9BSD8" . "E2F1 gene")
 ("HGNC:3113" . "E2F1 (human)")
 ("NCIT:C18379" . "E2F1 Gene")
 ("NCBIGene:1869" . "E2F transcription factor 1")
 ("UniProtKB:Q01094" . "E2F-1;")
 ("ENSEMBL:ENSG00000101412"
  .
  "E2F transcription factor 1 [Source:HGNC Symbol;Acc:HGNC:3113]")
 ("CUI:C0812258" . "E2f transcription factor 1")
 ("NCBIGENE:1869" . "E2F1 gene"))
"query.rkt">
|#


;; rhobtb2
(curie-synonyms/names "UMLS:C1425762")

#|
=>

(("UniProtKB:Q9BYZ6" . "RHOBTB2")
 ("UniProtKB:E5RI44" . "RHOBTB2")
 ("UMLS:C1425762" . "RHOBTB2 gene")
 ("NCBIGene:23221" . "Rho related BTB domain containing 2")
 ("HGNC:18756" . "RHOBTB2 (human)")
 ("NCBIGENE:23221" . "RHOBTB2")
 ("OMIM:607352" . "Rho-related btb domain-containing protein 2")
 ("ENSEMBL:ENSG00000008853"
  .
  "Rho related BTB domain containing 2 [Source:HGNC Symbol;Acc:HGNC:18756]")
 ("CUI:C1425762" . "Rho-related btb domain-containing protein 2"))
"query.rkt">
|#


;; failure to thrive
(curie-synonyms/names "HP:0001508")

#|
=>

(("MEDDRA:10047897" . "Weight gain poor")
 ("CUI:C0231246" . "Failure to gain weight")
 ("UMLS:C0231246" . "Failure to gain weight")
 ("CUI:C2315100" . "Pediatric failure to thrive")
 ("MEDDRA:10036164" . "Poor weight gain")
 ("UMLS:C2315100" . "Weight gain poor")
 ("HP:0001508" . "failure to thrive"))
|#


;; bcr
(curie-synonyms/names "UMLS:C0812385")

#|
=>

(("UniProtKB:H0Y554" . "BCR gene")
 ("UMLS:C0812385" . "BCR gene")
 ("NCBIGene:613" . "BCR activator of RhoGEF and GTPase")
 ("NCI:C18455" . "BCR gene")
 ("ENSEMBL:ENSG00000186716"
  .
  "BCR activator of RhoGEF and GTPase [Source:HGNC Symbol;Acc:HGNC:1014]")
 ("Orphanet:119016" . "BCR activator of RhoGEF and GTPase")
 ("NCBIGENE:613" . "BCR gene")
 ("NCIT:C18455" . "BCR Gene")
 ("UniProtKB:P11274" . "BCR")
 ("HGNC:1014" . "Bcr")
 ("OMIM:151410" . "Breakpoint cluster region")
 ("CUI:C0812385" . "Breakpoint cluster region"))
|#
