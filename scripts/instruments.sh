#!/usr/bin/env bash
set -eu

# cargo install --offline --path .

# export RUST_LOG=debug,swc_common=off
export RUST_BACKTRACE=1
export RUST_LOG=debug

# The type checker is buggy.
#
# (cd stc-lab/rxjs && stc check src/index.ts)

# declare module "*.less" is required
#
# (cd stc-lab/ant-design-mobile && stc check src/index.ts)


cargo profile instruments --release -t time --features tracing/release_max_level_off -- iterate $@
# cargo instruments --release -t time --features tracing/release_max_level_off -- check $@
# 