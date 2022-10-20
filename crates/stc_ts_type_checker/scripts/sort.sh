#!/bin/bash
set -eu
cat tests/conformance.pass.txt | awk NF | sort | uniq | awk '{$1=$1};1' | uniq | sort > tests/conformance_sorted.txt
mv tests/conformance_sorted.txt tests/conformance.pass.txt

cat tests/compiler.pass.txt | awk NF | sort | uniq | awk '{$1=$1};1' | uniq | sort > tests/compiler_sorted.txt
mv tests/compiler_sorted.txt tests/compiler.pass.txt

cat tests/tsc.wip.txt | awk NF | sort | uniq | awk '{$1=$1};1' | uniq | sort > tests/tmp.txt
mv tests/tmp.txt tests/tsc.wip.txt