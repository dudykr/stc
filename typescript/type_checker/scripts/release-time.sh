#!/usr/bin/env bash

set -eux

TEST='' cargo test --test tsc --release --features perf 2>&1 > /dev/null