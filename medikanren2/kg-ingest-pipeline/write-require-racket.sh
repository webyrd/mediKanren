#!/bin/bash

rfileRkt="$1"
afileOut="$2"

rm -f "$afileOut"
printf "#lang racket\n" >> "$afileOut"
printf "(require \"%s\")\n" "$rfileRkt" >> "$afileOut"
