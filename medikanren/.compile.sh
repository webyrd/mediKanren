#!/bin/bash
adirRepo="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"
adirMk="$adirRepo/medikanren"
adirArtifacts="$adirRepo/ci_artifacts"


stepname=medikanren1_compile_trapi
if raco make "$adirMk/apps/translator-web-server.rkt"
then
    echo "$stepname" > "$adirArtifacts/status/pass/$stepname"
else
    echo "$stepname" > "$adirArtifacts/status/fail/$stepname"
fi

stepname=medikanren1_compile_gui
if raco make "$adirMk/apps/gui-simple-v2.rkt"
then
    echo "$stepname" > "$adirArtifacts/status/pass/$stepname"
else
    echo "$stepname" > "$adirArtifacts/status/fail/$stepname"
fi
