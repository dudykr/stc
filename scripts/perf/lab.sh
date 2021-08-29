#!/usr/bin/env bash
set -eu

cargo install --offline --path .

export RUST_LOG=debug
export LD_LIBRARY_PATH="$(rustc --print sysroot)/lib"
# export RUST_BACKTRACE=1

# The type checker is buggy.
#
# (cd stc-lab/rxjs && stc check src/index.ts)

# declare module "*.less" is required
#
# (cd stc-lab/ant-design-mobile && stc check src/index.ts)


(cd stc-lab/jest/packages/jest-cli && time stc check src/index.ts)
