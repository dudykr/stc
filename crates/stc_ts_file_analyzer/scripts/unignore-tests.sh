#!/usr/bin/env bash
set -eu

find tests -type f -name '.DS_Store' -delete || true


find tests -type f -name '.*' | xargs -L 1 ./scripts/unignore-test.sh