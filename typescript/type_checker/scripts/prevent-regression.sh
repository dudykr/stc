#!/usr/bin/env bash
#
# A script to prevent regression.
# This scripts add all regressed lines to passing list.
#
set -eu

git diff --unified=0 origin/main -- tests/conformance.pass.txt \
    | grep '^[-]' \
    | sed 's/^.\{1\}//' \
    | grep -v '^[-]' >> tests/conformance.pass.txt

./scripts/sort.sh
