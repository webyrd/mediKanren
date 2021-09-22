#!/bin/bash
adirRepo="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"

# *** Pin versions of racket packages ***
#
# The racket central package server only records metadata for
# latest versions of packages.  This means that if any racket
# dependency were to push a breaking change, we would lose the abliity
# to continuously deploy.
#
# Packages of racket versions can be pinned using git, submodules
# and some arcane commands.  The procedure is documented at:
#
#   https://alex-hhh.github.io/2020/05/dependency-management-in-racket-applications.html
#
# At the end of the article, alex-hhh mentions setup-catalog.sh,
# which we reuse:
#
#   https://github.com/alex-hhh/ActivityLog2/blob/36a4bb8af45db19cea02e982e22379acb18d0c49/etc/scripts/setup-catalog.sh

afileToCheck="$adirRepo/pkgs/chk/README"
if [ ! -e "$afileToCheck" ]
then
    echo "*** ERROR"
    echo "  File $afileToCheck not found"
    echo "  Did you forget to initialize git submodules?"
    echo "  Hint:"
    echo "    git submodule init && git submodule update --recursive"
    exit 1
else
    rsync -rv --exclude-from="$adirRepo/pkgs/.rsync_exclude.txt" --exclude=aws/\* --delete "$adirRepo/pkgs/" "$adirRepo/pkgs-bin/"
    bash "$adirRepo/pkgs/setup-catalog/setup-catalog.sh" "$adirRepo/pkgs-bin"
    raco pkg remove --auto mediKanren-dependencies
    raco pkg install --batch --auto mediKanren-dependencies
    for adir in "$adirRepo"/pkgs/*
    do
	if [ -e "$adir/.git" ]
	then
	    echo cleaning submodule $adir
	    # If we don't clean, git submodules will have dirty
	    # status after installing packages, which might make
	    # it easier to get confused and accidentally upgrade
	    # a package.
	    (cd "$adir" && git clean -f -d)
	fi
    done
fi

