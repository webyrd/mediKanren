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
* Optionally, map pubmed ids to the edge ids that reference them: `racket build-pubmed-edges.rkt data NAME`
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

### Back up CSV source data

If CSVs are downloaded from a remote source, then after the CSVs are grouped in a directory, yet before running racket conversion scripts, first create a zip for backup:
```
cd data
zip -r semmed.csv.zip semmed
```

This isn't necessary for neo4j dumps because the dump is a reliable source (though doing so could still save the self-download time).  Remote sources are not reliable.

To backup the CSV->mediKanren work:
```
cd data
zip -r robokop.db.zip robokop
```


## Using a local neo4j instance

If you were given a dump file instead of a remote neo4j instance to connect to, start here.
Follow these instructions to set up a local instance using the dump file.  Then follow the usual ingestion instructions.

### Start a local neo4j instance from a dump file

Assuming `neo4j` is already stopped, convert a Robokop (or RTX) `X.dump` to CSV format via:

```
neo4j-admin load --from X.dump --database graph.db --force  # If this is your first time, remove the --force option.
neo4j start
cd YOUR-PATH-TO-NCATS-ROOT/ncats-translator/kgx
cp config-robokop.yml config.yml
time python3 neo4j_download.py
neo4j stop
```

Then follow the instructions for converting the CSVs to mediKanren format.

(If you've just set neo4j up for the first time and kgx failed due to authorization, try visiting localhost:7474 in the browser, and logging in with username=neo4j password=neo4j.  You'll be prompted to set a new password.  Set it to be consistent with the password in your kgx config file.)

### Freeing up space once you are finished

If you installed neo4j using homebrew:

```
neo4j stop
# Your data might live here.
rm -rf /usr/local/Cellar/neo4j/VERSION/libexec/data/databases/*
# Or it might live here.
rm -rf /usr/local/var/neo4j/data/databases/*
```

Supposedly this query should also work, but I get a memory error:

```
neo4j start
cypher-shell -u YOURUSERNAME -p YOURPASSWORD --non-interactive 'MATCH(n) DETACH DELETE n'
neo4j stop
```


## TODO

* reorganize directory structure
  * lift biolink to main directory, moving current main contents somewhere else
  * keep configs, logs,  and user programs (such as gui, web-server...) in main directory
  * move library and data processing code to a new subdirectory
  * move tests and examples into their own subdirectories

* generalize database representation and indexing
  * choose representation based on field cardinalities
    * store representation metadata per-database
  * make it possible to index using arbitrary fields
    * declaratively specify desired indices
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


### bottom-up explorer and graph builder ideas
* organization
  * all work persistsed in a single environment/repo/version-DAG
  * workspaces: can have multiple pointers/views/HEADs into the environment
    * expressed as separate workspaces/tabs/splits to support concurrent
      activities
      * not a perfect analogy since you might want multiple visual windows
        into the same workspace for UI convenience
    * manipulating multiple workspaces is analogous to branching
      * opening a new empty workspace is analogous to creating a new branch
        at the (empty) "initial commit"
  * while "branches" are mutable in the sense that they update/repoint as
    manipulations are performed, data itself is stateless/versionless: may
    copy/reference data across worksapces
  * environment is version-controlled at two levels
    * fine-grained event log recording all user manipulations automatically
      * raw diffs: show true manipulation history
      * algebraically simplified diffs: show only effective manipulations
        * if user flips back and forth between two states, cancel them out
      * filtered diffs (raw or simplified): only show manipulations relevant
        to a subset of workspace components
    * course-grained commits/tags/bookmarks that the user explicitly creates
    * support rebase/merge/cherry-picking, with optional component filtering

* data
  * concept sets
    * unknown or union/intersection/difference of other sets
    * filter if a text search query is given
    * filter if known category
    * filter if constrained as source/target with given predicates
    * filter by user selections
  * predicate sets (e.g., increases, decreases)
    * unknown or union/intersection/difference of other sets
    * filter if a text search query is given
    * filter if known parent class(es)
    * filter if constrained by given source/target concepts
    * filter by user selections
  * graphs
    * nodes and edges
      * nodes constrained by concept sets
      * edges connect subject and object sets, constrained by predicate sets
      * metadata: unique ID, optional name, UI, or visualization preferences
      * knowns
        * stratified concept/predicate set computation
      * unknowns
        * nodes collect solution sets of concepts
        * edges collect solution sets of triples
    * subgraphs
      * used for organizational convenience
      * metadata
      * collection of nodes and edges
      * may compute as union/intersection/difference of other subgraphs

* computation
  * stratified construction and constraint resolution
    * topologically sort element sets along construction expression dependencies
    * iteratively compute knowns and unknowns
      * compute known concept and predicate sets
        * construct new sets using dependencies
        * filter elements by class, then text search, then selections
        * validate element selections
      * solve for unknowns used as subject/object, i.e., (== #f construction)
        * filter subject/object by text search, then class, then selections
        * find edge triples
        * accumulate subject/object concepts from triples, making them known
        * validate subject/object selections
  * cache retrieved triples per workspace?

* basic graph building manipulations
  * update node/edge metadata
    * node/edge attributes, UI position, other preferences
  * create new node/edge
  * duplicate subgraph
    * alpha rename components (maintain unique ID invariant)
    * retain dependencies on external component
      * i.e., computations and constraints
    * create new internal dependencies that preserve relationships between
      subgraph components
  * connect nodes and edges
  * add/remove node/edge constraints
    * choose a set of categories/predicate-classes
    * compute any union/intersection/difference operations
    * search for text (across name, CURIE, description, etc.)
    * manually select entries from a list

* more advanced graph building manipulation ideas
  * multi-hop path-finding between concepts, discovering concepts in between
  * concept discovery/introduction prioritized via relevance to a background
    context
    * background context specified with concept sets
    * introduced concepts prioritized by the presence of more/better
      connections to context concepts

* UI
  * a text-based interface usable from the command-line
    * workspace could be a directory structure of subgraphs
  * a (web-based?) graphical interface
    * clicking/dragging and spatial visualization
