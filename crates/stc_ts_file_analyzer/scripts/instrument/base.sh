#!/usr/bin/env bash
set -eu

# This script instruments debug build, because this script exists to make development faster

export RUST_LOG=off
export MIMALLOC_SHOW_STATS=1

cargo profile instruments -t time --features swc_common/concurrent --test base $@