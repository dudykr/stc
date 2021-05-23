#!/usr/bin/env bash
set -eu


TEST_FILES=$(find ./tests/pass -type f -name "*.ts")

for F in $TEST_FILES
do
    if tsc --strict --noEmit $F ; then
        echo "$F is a valid pass test"
    else
        ./scripts/test/move-pass-test.sh $F
    fi
done


