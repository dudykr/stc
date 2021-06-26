#!/usr/bin/env bash
set -eux

TEST="$1" cargo test --test tsc --release -- --nocapture