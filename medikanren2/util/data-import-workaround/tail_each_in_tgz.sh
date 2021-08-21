#!/bin/bash
adirScript="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
set -e

numLines="$1"
rfileIn="$2"
rfileOut="$3"
afileScript="$adirScript/tail_each_in_tgz.sh"

log() {
    echo "$1" 1>&2
}

usage() {
    log "Usage:"
    log "  bash tail_each_in_tgz.sh <number of lines> <input tgz file> <output tgz file>"
}

checkArgsMain() {
    if [ "$rfileIn" == "" ]
    then
        log "error 1"
        usage
        return 1
    elif [ "$rfileOut" == "" ]
    then
        log "error 2"
        usage
        return 1
    elif [ "$numLines" == "" ]
    then
        log "error 3"
        usage
        return 1
    elif [ "$(( $numLines + 0 ))" != "$numLines" ]
    then
        log "error 4"
        usage
        return 1
    elif [ -z "$rfileIn" ]
    then
        log "File not found: $rfileIn"
        return 1
    fi
    return 0
}

adirTmp="/data/tmp"

if checkArgsMain
then
    #rm -rf "$adirTmp"
    mkdir -p "$adirTmp"
    for rfile in $(tar tzf "$rfileIn")
    do
        echo "truncating $rfile"
        # TODO to support directories, mkdir -p "/$adirTmp/$(dirname "$rfile")"
        tar xzf "$rfileIn" --to-stdout "$rfile" | head -n"$numLines" > "$adirTmp/$rfile"
    done
    tar czf "$(pwd)"/"$rfileOut" -C "$adirTmp" .
    # TODO cleanup tmp
fi
