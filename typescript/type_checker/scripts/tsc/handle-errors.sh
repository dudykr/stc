#!/usr/bin/env bash

set -eu

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
npx tsc $DIR/handle-errors.ts
node $DIR/handle-errors.js