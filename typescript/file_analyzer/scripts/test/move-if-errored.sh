#!/usr/bin/env bash
set -eu


TEST_FILES=$(find ./tests/pass -type f -name "*.ts")

for F in $TEST_FILES
do
    if tsc --strictFunctionTypes --noImplicitAny --noEmit --downlevelIteration $F ; then
        echo "$F is a valid pass test"
    else
        ./scripts/test/move-pass-test.sh $F
        # code $F
    fi
done


