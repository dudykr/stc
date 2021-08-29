#!/usr/bin/env bash
set -eu

cargo install --offline --path .

export LD_LIBRARY_PATH="$(rustc --print sysroot)/lib"

# The type checker is buggy.
#
# (cd stc-lab/rxjs && stc check src/index.ts)

(cd stc-lab/ant-design-mobile && stc check src/index.ts)

