#!/usr/bin/env bash

set -eu

cp -R ~/projects/TypeScript/tests/cases/conformance/ ./tests/conformance/

mkdir -p ~/projects/stc/typescript/checker/tests/reference/

# find ~/projects/TypeScript -name '*.errors.txt' -exec cp {} tests/reference/ \;
find ~/projects/TypeScript -name '*.types' -exec cp {} tests/reference/ \;
