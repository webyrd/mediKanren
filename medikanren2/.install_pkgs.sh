#!/bin/bash
adirRepo="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"

git submodule init
git submodule update
bash "$adirRepo/pkgs/setup-mediKanren-pkgs.sh"
