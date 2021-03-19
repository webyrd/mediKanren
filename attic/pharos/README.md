# mediKanren Pharos Interface

This is a mediKanren interface into the data provided by Pharos: https://pharos.nih.gov/idg/about

Currently, we only provide tools for converting the Pharos DB to SQLite3 for interactive use.


## Setup

### Download and install SQLite

You can find this at: https://www.sqlite.org/download.html

Your OS's package manager may provide a more appropriate version.


### Obtain a MySQL dump of the Pharos DB

Download it from: http://juniper.health.unm.edu/tcrd/download/

The file to download will have a name with either the form `tcrd_vX.Y.Z.sql.gz` or `latest.sql.gz`, depending on what version of the data you want.

NOTE: though the filename ends with `.gz`, it's probably not gzipped.


### Convert MySQL dump to a SQLite DB

Given `data.sql.gz`

Run: `./pharos-mysql-to-sqlite3-to-db data.sql.gz`

This may take a while to finish, as there are two transformation steps being performed on a large file.


## Use

### Interact with SQLite DB

Run: `./interact-sqlite3`

Interact using SQL and dot-commands.

By default, output is in CSV format with headers and command echoing.

To write output to a file instead of stdout, issue the dot-command: `.output your-file-name.csv`


## Process SQLite DB

Convert SQLite DB to an internal form usable by mediKanren queries.

### Output schema.sql

Run: `./interact-sqlite3`

Then use the following commands to output the `schema.sql` file.

```
.output schema.sql
.schema
.exit
```

### Tokenize schema.sql -> schema-tokens.scm

`racket sql-tokenize.rkt < schema.sql > schema-tokens.scm `

### Parse schema-tokens.scm -> schema.scm

`racket sql-schema-parse.rkt < schema-tokens.scm > schema.scm`

### Translate SQLite DB to internal format

From the `pharos` directory, run: `racket sqlite-to-db.rkt schema.scm data`


## Interact using miniKanren

`mk-db.rkt` provides the `pharos-metao` and `pharoso` relations.

```
(require "mk-db.rkt")

(run 10 (table-name metadata)
  (pharos-metao table-name metadata))

(run 20 (tuple)
  (fresh (p q) (== `(,p 19295 . ,q) tuple))
  (pharoso "gene_attribute" tuple))
```
