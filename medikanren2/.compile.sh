#!/bin/bash
if [ -z "$adirRepo" ]
then
    echo -e "*** ERROR ***\n  Invoke with \".\" or \"source\" from another bash script\n" 1>&2
else
    stepname=medikanren2_compile_trapi
    if (cd "$adirMk" && raco make "server.rkt" ${rfileTests} )
    then
        echo "$stepname" > "$adirArtifacts/status/pass/$stepname"
    else
        echo "$stepname" > "$adirArtifacts/status/fail/$stepname"
    fi
fi