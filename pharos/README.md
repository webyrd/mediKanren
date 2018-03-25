# mediKanren Pharos Interface

This is a work in progress.  Currently, we only provide tools for converting the Pharos DB to SQLite3 for interactive use.


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


## Process SQLite Schema

This section is still incomplete.

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
