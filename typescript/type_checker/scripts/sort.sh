#!/bin/bash
set -eu
cat tests/conformance.pass.txt | awk NF | sort | uniq | awk '{$1=$1};1' | uniq | sort > tests/conformance_sorted.txt
mv tests/conformance_sorted.txt tests/conformance.pass.txt