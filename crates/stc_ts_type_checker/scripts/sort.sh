#!/bin/bash
set -eu

function sortFile {
    cat $1 | awk NF | sort | uniq | awk '{$1=$1};1' | uniq | sort > tests/tmp.txt
    mv tests/tmp.txt $1
}

sortFile tests/conformance.pass.txt
sortFile tests/compiler.pass.txt
sortFile tests/tsc.wip.txt
sortFile tests/tsc.ignored.txt