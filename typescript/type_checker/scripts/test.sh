#!/usr/bin/env bash
set -eux

./scripts/sort.sh

export RUST_BACKTRACE=1

# # We use visualizer to see actual types while working on conformance.
# # So existence of failing visualization test typically means that we added a new fixture.
# git restore -- ../file_analyzer/tests/visualize
# UPDATE=1 cargo test -p stc_ts_file_analyzer --test visualize

cargo test --color always -q --test conformance
