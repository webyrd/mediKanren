#!/bin/bash
set -eufo pipefail

# To download or create Chinook_Sqlite.sql, see: https://github.com/lerocha/chinook-database

here="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd "$here"

dos2unix Chinook_Sqlite.sql
sqlite3 -init Chinook_Sqlite.sql chinook.sqlite < /dev/null

for table in Customer Employee Invoice InvoiceLine Genre Album Artist Playlist PlaylistTrack Track MediaType; do
  printf ".mode tabs\n.headers on\n.output $table.tsv\nselect * from $table;" | sqlite3 chinook.sqlite
done

rm chinook.sqlite
