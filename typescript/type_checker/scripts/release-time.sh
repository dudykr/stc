#!/usr/bin/env bash

set -eux

export RUST_LOG=off

TEST="$1" cargo test --test tsc --release