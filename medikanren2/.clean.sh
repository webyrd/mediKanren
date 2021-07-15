#!/bin/bash
adirRepo="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"
adirMk="$adirRepo/medikanren2"

set -o pipefail
touch "$adirMk"/junk.zo # avoid failure when already clean
if ! find "$adirMk" -name \*.zo | xargs -L1 rm
then
  echo "*** Warning *** .zo file deletion may not have been complete (e.g. due to spaces in filenames)"
fi
