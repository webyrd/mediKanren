#!/bin/bash
adirRepo="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"

bash "$adirRepo/pkgs/setup-mediKanren-pkgs.sh"
