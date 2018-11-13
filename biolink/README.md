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
* Modify `config.yml` to use the instance information you obtained.  Do not include the `bolt://` prefix in the hostname, as it's already assumed.
* Modify `neo4j_download.py` to give an appropriate name to the files that will be downloaded.  Currently, the string `semmed` is being passed to the `CsvSink`.  Change this string to change the name.
* Run the download script via python3, likely with this command: `python3 neo4j_download.py` (For a typical data source, the download may take an hour or so.)

### Converting CSVs to mediKanren format

* `cd` to the `biolink` subdirectory of the `mediKanren` repo (which is this repo).
* Move or copy the downloaded CSV files to an appropriately-named subdirectory of the `biolink/data` directory.  We'll assume your datasource is named `NAME`, and will live at `biolink/data/NAME`.
* Perform conversion by running: `racket csv-graph-to-db.rkt NAME` (For a typical data source, conversion may take an hour or so.)
* Given `(require "mk-db.rkt")` you should now be able to access the new db by evaluating `(make-db "db/NAME")`.

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
