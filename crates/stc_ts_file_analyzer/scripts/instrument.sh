#!/usr/bin/env bash
set -eu

cargo instruments --release -t time --features tracing/release_max_level_off --example file $@