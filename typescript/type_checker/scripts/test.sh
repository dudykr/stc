#!/usr/bin/env bash
set -eux


# We use visualizer to see actual types while working on conformance.
# So existence of failing visualization test typically means that we added a new fixture.
cargo test --test visualize

cargo test --test conformance
