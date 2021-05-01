#!/usr/bin/env bash

set -eu

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
ts-node $DIR/handle-errors.ts

# Ignore tests with multiple result.
find ./tests/conformance -name "*\(*" -print \
    | sed 's!./tests/conformance/!!' \
    | sed 's!(.*!!' \
    >> tests/conformance.ignored.txt