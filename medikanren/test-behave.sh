#!/bin/bash
set -euo pipefail

## Usage: bash test-behave.sh PATH-TO-TRANSLATOR-TESTING-FRAMEWORK

here="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

: ${output_path="$here/test-output.txt"}
: ${behave_path="$1"}

printf 'output_path=%s\n' "$output_path"
printf 'behave_path=%s\n' "$behave_path"

trap 'jobs > /dev/null && kill $(jobs -p)' EXIT ERR

while true; do
  printf "Tests running: %s\n" "$(date)"
  cd "$here"
  racket translator-web-server.rkt &> "$output_path" &
  sleep 120
  cd "$behave_path"
  ( time behave -i features/medikanren-tests.feature --no-capture --no-capture-stderr
  ) &>> "$output_path" || cat "$output_path"
  printf 'Tests finished: %s\n' "$(date)"
  kill $(jobs -p) || true
  sleep 60
done
