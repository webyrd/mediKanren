# Biolink interface

[Biolink model](https://biolink.github.io/biolink-model/)


## Setup

* Install racket.
* Install python3.
* Clone this repo.
* Clone the kgx repo: https://github.com/NCATS-Tangerine/kgx
* Checkout the `source-sink` branch in kgx: `git checkout origin/source-sink`


## Ingesting new data sources

### Downloading neo4j data in CSV format

* Obtain username, password, host, and port of the neo4j instance.
* `cd` to the kgx repo.
* Modify `config.yml` to use the instance information you obtained.  Define an appropriate `outputname` here.
* Run the download script via python3, likely with this command: `python3 neo4j_download.py` (For a typical data source, the download may take an hour or so.)

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

* index concepts by `CUI`, including a fuzzy lookup
* index edges by predicate
* map Robokop pubmed ids to edge ids

* web interface
  * webserver endpoints for lookup of:
    * concepts/predicates (by name or CUI)
    * Xs (by chosen concepts and predicates)
  * web client corresponding to GUI
