#!/usr/bin/env bash

set -eu

cp -R ~/projects/TypeScript/tests/cases/conformance/ ./tests/conformance/
cp -R ~/projects/TypeScript/tests/cases/compiler/ ./tests/compiler/

find ./tests/conformance/ -type f | xargs dos2unix
find ./tests/compiler/ -type f | xargs dos2unix

mkdir -p ~/projects/stc/typescript/checker/tests/reference/

find ~/projects/TypeScript -name '*.errors.txt' -exec cp {} tests/reference/ \;
# find ~/projects/TypeScript -name '*.types' -exec cp {} tests/reference/ \;

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ts-node $DIR/handle-errors.ts