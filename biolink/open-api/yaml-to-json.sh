#!/bin/bash
set -eufo pipefail

here="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

python3 "$here/yaml2json.py" < "$here/TranslatorReasonersAPI.yaml" > "$here/TranslatorReasonersAPI.json"
