#!/bin/bash
adirRepo="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"
adirMk="$adirRepo/medikanren2"
adirArtifacts="$adirRepo/ci_artifacts"

# Name files -spec.rkt to be auto-discovered as automated unit tests for the 10GB-CI run.

rfileTests=$(cd "$adirMk" && \
    find . -name \*-spec.rkt -a \
        \( -path \*/test/10GB-CI/\* -o -not -path \*/test/10GB-CI/\* \) )

bash "$adirMk/.clean.sh"
. "$adirMk/.compile.sh"

find "$adirMk" -name config\*.scm

echo "$adirMk/etc/config.installer.scm:"
cat "$adirMk/etc/config.installer.scm"

if (cd "$adirMk" && env MK_STAGE=prod raco test ${rfileTests} )
then
    echo medikanren_run_ci > "$adirArtifacts/status/pass/medikanren2_run_ci"
else
    echo medikanren_run_ci > "$adirArtifacts/status/fail/medikanren2_run_ci"
fi
