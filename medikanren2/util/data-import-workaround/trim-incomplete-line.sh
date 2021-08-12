#!/bin/bash
adirData="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd ../../data && pwd )"
set -e

rdir="$1"
rfile="$2"

afileGood="${adirData}/${rdir}/${rfile}"
afileBad="${adirData}/${rdir}/${rfile}.corrupt"

if [[ ! -e "$afileGood" ]]
then
    echo "file not found: $afileGood" 1>&2
    exit 1
else
    rm -f "$afileBad"
    mv "$afileGood" "$afileBad"
    head -n-2 "$afileBad" > "$afileGood"
fi
