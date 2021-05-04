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


# TODO: Replace this way to run tests with a way that actually
# discovers them.  The purpose of the current way only to show that
# tests can be run in Github Actions against real data.

if raco test "$adirMk/configref.rkt"
then
    echo medikanren_run_ci > "$adirArtifacts/status/pass/medikanren1_run_ci"
else
    echo medikanren_run_ci > "$adirArtifacts/status/fail/medikanren1_run_ci"
fi


