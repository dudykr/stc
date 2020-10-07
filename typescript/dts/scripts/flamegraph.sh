#!/usr/bin/env bash
set -eux


sudo CARGO_MANIFEST_DIR=. cargo flamegraph --test fixture