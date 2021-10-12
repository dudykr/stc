#!/usr/bin/env bash
set -eu

# cargo install --offline --path .

# export RUST_LOG=debug,swc_common=off
# export RUST_BACKTRACE=1

# The type checker is buggy.
#
# (cd stc-lab/rxjs && stc check src/index.ts)

# declare module "*.less" is required
#
# (cd stc-lab/ant-design-mobile && stc check src/index.ts)


cargo instruments --release -t time --features tracing/release_max_level_off -- check ~/projects/stc-lab/jest/packages/jest-cli/src/index.ts
cargo instruments --release -t time --features tracing/release_max_level_off -- check $@
# 