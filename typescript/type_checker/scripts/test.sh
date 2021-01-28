#!/usr/bin/env bash
set -eux

./scripts/sort.sh

export RUST_BACKTRACE=1

# We use visualizer to see actual types while working on conformance.
# So existence of failing visualization test typically means that we added a new fixture.
git restore -- tests/visualize
UPDATE=1 cargo test --test visualize

cargo test --color always -q --test conformance
