#!/bin/bash

afileRkt="$1"
afileOut="$2"

rm -f "$afileOut"
printf "#lang racket\n" >> "$afileOut"
printf "(require \"%s\")\n" "$afileRkt" >> "$afileOut"
