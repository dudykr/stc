#!/usr/bin/env bash
set -eu

function sortFile {
    mkdir -p .stc
    cat $1 | awk NF | sort | uniq | awk '{$1=$1};1' | uniq | sort > .stc/tmp.txt
    mv .stc/tmp.txt $1
}

export GIST_DIR="$(cd 8198130f16e42514b22656c57690b124 && pwd)"
echo "Gist dir: $GIST_DIR"

# Clone https://gist.github.com/kdy1/8198130f16e42514b22656c57690b124 to your CDPATH
(cd $GIST_DIR && git pull || true)
(cd $GIST_DIR && sortFile done.txt)

find crates/* -name "*.stats.rust-debug" \
    | xargs grep 'extra_error: [1-9][0-9]*' \
    | sort -n -k 3 -t ":" -r \
    > .stc/all-tests-to-split.txt