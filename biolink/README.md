# Biolink interface

[Biolink model](https://biolink.github.io/biolink-model/)


## Setup

* Install racket.
* Install python3.
* Clone this repo.
* Clone the kgx repo: https://github.com/NCATS-Tangerine/kgx
* Checkout the `source-sink` branch in kgx: `git checkout origin/source-sink`
* Install dependencies via: `pip3 install -r requirements.txt`


## Ingesting new data sources

### Downloading neo4j data in CSV format

* Obtain username, password, host, and port of the neo4j instance.
* `cd` to the kgx repo.
* Modify `config.yml` to use the instance information you obtained.  Set the `outputname` here to describe your data source, e.g., `robokop`.
* Run the download script via `python3`, likely with this command: `python3 neo4j_download.py` (For a typical data source, the download may take an hour or so.)

### Converting CSVs to mediKanren format

* `cd` to the `biolink` subdirectory of the `mediKanren` repo (which is this repo).
* Move or copy the downloaded CSV files to an appropriately-named subdirectory of the `biolink/data` directory.  We'll assume your datasource is named `NAME`, and will live at `biolink/data/NAME`.
* Perform conversion by running: (For a typical data source, conversion may take an hour or so.)
```
racket csv-graph-to-db.rkt data NAME
racket build-string-index.rkt data NAME
```
* Optionally, map pubmed ids to the edge ids that reference them (currently only applicable to semmed): `racket build-pubmed-edges.rkt data NAME`
  * NOTE: generalize for Robokop:
    * Semmed pubs look like: ("pmids" . "24021883;24021883")
    * Robokop pubs look like: ("publications" . "(\"PMID:15917307\" \"PMID:25451572\")")
* Given `(require "mk-db.rkt")` you should now be able to access the new db by evaluating `(make-db "data/NAME")`.

### Verify data

Within racket, running this should produce sensible results:
```
(require "mk-db.rkt")
(define NAME (make-db "data/NAME"))
(run* (c) (db:categoryo NAME c)))
(run* (p) (db:predicateo NAME p)))
(run 10 (c) (db:concepto NAME c)))
(run 10 (e) (db:edgeo NAME e)))
```


## TODO

* reorganize directory structure
  * lift biolink to main directory, moving current main contents somewhere else
  * keep configs, logs,  and user programs (such as gui, web-server...) in main directory
  * move library and data processing code to a new subdirectory
  * move tests and examples into their own subdirectories

* map Robokop pubmed ids to edge ids
* index edges by predicate
* index concepts by CUI, including synonyms
  * robokop: `"equivalent_identifiers"`
  * orange: `"same_as"`
  * semmed: `"xrefs"`

```
  (robokop 26 "HGNC:1100" "BRCA1" (0 . "(\"named_thing\" \"gene\")") (("locus_group" . "protein-coding gene") ("chromosome" . "17") ("taxon" . "9606") ("gene_family" . "(\"Ring finger proteins\" \"FA complementation groups\" \"Protein phosphatase 1 regulatory subunits\" \"BRCA1 A complex\" \"BRCA1 B complex\" \"BRCA1 C complex\")") ("location" . "17q21.31") ("id" . "HGNC:1100") ("gene_family_id" . "(58 548 694 1328 1335 1336)") ("equivalent_identifiers" . "(\"UniProtKB:C9IZW4\" \"UniProtKB:E9PC22\" \"UniProtKB:A0A2R8Y7V5\" \"UniProtKB:H0Y8D8\" \"UniProtKB:E9PH68\" \"UniProtKB:K7EPC7\" \"UniProtKB:E7EQW4\" \"UniProtKB:H0Y881\" \"UniProtKB:E7EWN5\" \"UniProtKB:H0Y850\" \"UniProtKB:C6YB45\" \"UniProtKB:E7EUM2\" \"UniProtKB:A0A024R1V0\" \"HGNC:1100\" \"UniProtKB:A0A0U1RRA9\" \"UniProtKB:E7ENB7\" \"UniProtKB:K7EJW3\" \"UniProtKB:H0Y8B8\" \"UniProtKB:A0A2R8Y6Y9\" \"UniProtKB:Q5YLB2\" \"UniProtKB:P38398\" \"UniProtKB:B7ZA85\" \"UniProtKB:A0A0A0MSN1\" \"ENSEMBL:ENSG00000012048\" \"UniProtKB:Q3B891\" \"UniProtKB:G1UI37\" \"NCBIGENE:672\" \"UniProtKB:A0A2R8Y587\")")))
  (orange 32553 "NCBIGene:672" "BRCA1" (6 . "(\"gene\")") (("iri" . "http://www.ncbi.nlm.nih.gov/gene/672") ("synonym" . "(\"BRCA1/BRCA2-containing complex, subunit 1\" \"Fanconi anemia, complementation group S\" \"protein phosphatase 1, regulatory subunit 53\" \"BRCC1\" \"FANCS\" \"PPP1R53\" \"RNF53\" \"BREAST CANCER 1 GENE; BRCA1\" \"BRCA1\")") ("in_taxon" . "NCBITaxon:9606") ("same_as" . "(\"ENSEMBL:ENSG00000012048\" \"HGNC:1100\" \"OMIM:113705\" \"Orphanet:119068\")") ("provided_by" . "(\"orphanet.ttl\" \"omim.ttl\")") ("description" . "BRCA1, DNA repair associated") ("id" . "NCBIGene:672")))

  (semmed 74686 "UMLS:C0376571" "BRCA1 gene" (4 . "gene") (("umls_type_label" . "['Gene or Genome']") ("xrefs" . "['NCI_NCI-HGNC:HGNC:1100', 'CHV:0000031821', 'PDQ:CDR0000043111', 'MESH:D019398', 'CSP:4005-0006', 'MTH:NOCODE', 'LNC:LP36227-4', 'NCI:C17965', 'LNC:LP19666-4', 'OMIM:113705', 'HGNC:HGNC:1100']") ("id" . "UMLS:C0376571") ("umls_type" . "['T028']") ("labels" . "['gene']")))
```

* web interface
  * webserver endpoints for lookup of:
    * concepts/predicates (by name or CUI)
    * Xs (by chosen concepts and predicates)
  * web client corresponding to GUI

* try automatic goal reordering based on cardinality statistics
