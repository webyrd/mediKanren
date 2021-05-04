#!/bin/bash
adirRepo="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"
adirMk="$adirRepo/medikanren2"
adirArtifacts="$adirRepo/ci_artifacts"

stepname=medikanren2_compile_trapi
if raco make "$adirMk/server.rkt"
then
    echo "$stepname" > "$adirArtifacts/status/pass/$stepname"
else
    echo "$stepname" > "$adirArtifacts/status/fail/$stepname"
fi
