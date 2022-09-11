# mediKanren 2 neo TRAPI server

This directory contains the new, or `neo`, server for mediKanren 2, supporting the Translator Reasoner API (TRAPI) described at https://github.com/NCATSTranslator/ReasonerAPI


## Racket install and raco package install

Like the rest of mediKanren, the neo server requires that [DrRacket](https://racket-lang.org) be installed.

You may receive an error from Racket when trying to start the neo server, saying that a required package is missing.  If so, you will need to install the package using the `raco` command-line tool.  For example, to install the `yaml` package, you can run the `raco` command:

```
raco pkg install yaml
```

(You will need to ensure that `raco` is on your `$PATH`)

You can see the packages already installed using `raco pkg show`.  On Will's laptop, these packages are installed:

```
webyrd@Williams-MacBook-Pro neo % raco pkg show
Installation-wide:
 Package            Checksum             Source
 main-distribution  85e547d3dea60fc6...  catalog...tribution
 racket-lib         ce8f1f745fc8c148...  catalog racket-lib
 [205 auto-installed packages not shown]
User-specific for installation "8.5":
 Package         Checksum              Source
 aws             94a16a6875ac585a1...  catalog...aws/master
 chk             32fb635e19fa2dc2d...  catalog...k?path=chk
 memoize         02a647d7fec308f84...  catalog...ize#master
 shell-pipeline  116529759e06c850b...  catalog...l-pipeline
 yaml            b60a1e4a01979ed44...  catalog...aml/master
 [7 auto-installed packages not shown]
```

## dbKanren

The neo server uses the the dbKanren engine from `https://github.com/gregr/dbKanren`

For now, a snapshot of dbKanren is included in the top-level `neo` directory.  It may be better to use a symlink and to checkout from `git` a specific version of dbKanren.  For simplicity of deployment, though, it is easier to just include the correct `dbKanren` code locally.


## Basic organization

TODO


## Data and 

The neo server expects processed data to be located in the `neo-data` directory:

`mediKanren/medikanren2/neo/neo-data/`


## Processing of raw downloaded 2-TSV-file KGX formatted KGs from KGE

To process a "raw" 2-TSV-file KGX formatted KGs from KGE, first download the and uncompress the KG in the

`neo-data/raw_downloads_from_kge_archive/`

directory.  For example,

`neo-data/raw_downloads_from_kge_archive/rtx-kg2pre_7.6/`


Next, please read and follow the instructions in 

`./neo-data-import/an_important_note_on_pre_processing_kgs.txt`

before running any of the scripts in `./neo-data-import/transform-2tsv-to-4tsv-kgs/` on a KG, to detect and fix a possible problem with DOS-style carriage returns (^M) that can appear in some of the KG TSVs.  This is critical, in order to avoid import errors.


Then, running the transformation script in `neo/neo-data-import/transform-2tsv-to-4tsv-kgs/` to generate a directory containing 4 TSV files, which in turn can be processed using the scripts in `neo/neo-data-import/build-mediKanren2-kg-from-4tsv/`.

For example, running the script

`./neo-data-import/transform-2tsv-to-4tsv-kgs/transform-rtx-kg2pre_7.6.rkt`

will transform the 2 TSV `rtx-kg2pre_7.6` files here:

```
neo-data/
  raw_downloads_from_kge_archive/
    rtx-kg2pre_7.6/
      edges.tsv
      nodes.tsv
      ...
```

into the 4 TSV files here:

```
neo-data/
  raw_downloads_from_kge_archive_transformed_to_4tsv/
    rtx-kg2pre_7.6/
      rtx-kg2pre_7.6.edge.tsv
      rtx-kg2pre_7.6.edgeprop.tsv
      rtx-kg2pre_7.6.node.tsv
      rtx-kg2pre_7.6.nodeprop.tsv
```

Afterwards, running the script

`./neo-data-import/build-mediKanren2-kg-from-4tsv/import-rtx-kg2pre_7.6.rkt`

will generate the fully-processed dbKanren database here:

```
neo-data/
  rtx-kg2pre_7.6.db/
```


## Starting the server

TODO

`neo-server.rkt` in `./neo-server/`


`racket neo-server.rkt`

```
> (require "neo-server.rkt")
```

```
> ,en neo-server.rkt
```

```
> (stop)
```

```
> (define stop (serve DEFAULT_PORT))
```

```
> (stop)
```
