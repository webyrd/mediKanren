#!/bin/bash

find / -maxdepth 2 -type d
df -k

echo About to start mediKanren TRAPI service
/usr/bin/racket /local/medikanren2/server.rkt
