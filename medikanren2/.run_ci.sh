#!/bin/bash
adirRepo="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"
adirMk="$adirRepo/medikanren2"
adirArtifacts="$adirRepo/ci_artifacts"

echo medikanren2_run_ci > "$adirArtifacts/status/pass/medikanren2_run_ci"

