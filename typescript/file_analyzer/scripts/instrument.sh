#!/usr/bin/env bash
set -eu

cargo instruments --release -t time --open --example perf