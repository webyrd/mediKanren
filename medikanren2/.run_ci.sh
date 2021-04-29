#!/bin/bash
adirRepo="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"
adirMk="$adirRepo/medikanren2"
adirArtifacts="$adirRepo/ci_artifacts"

# TODO: Replace this way to run tests with a way that actually
# discovers them.  The purpose of the current way only to show that
# tests can be run in Github Actions against real data.

if raco test "$adirMk/test/10GB-CI/with-yeast-sri-reference-kg.rkt"
then
    echo medikanren_run_ci > "$adirArtifacts/status/pass/medikanren2_run_ci"
else
    echo medikanren_run_ci > "$adirArtifacts/status/fail/medikanren2_run_ci"
fi

