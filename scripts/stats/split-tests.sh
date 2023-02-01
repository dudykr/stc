#!/usr/bin/env bash
set -eu

# Clone https://gist.github.com/kdy1/8198130f16e42514b22656c57690b124 to your CDPATH
(cd 8198130f16e42514b22656c57690b124 && git pull || true)


find crates/* -name "*.stats.rust-debug" | xargs grep 'extra_error: [1-9][0-9]*' | sort -n -k 3 -t ":" -r > .stc/all-tests-to-split.txt