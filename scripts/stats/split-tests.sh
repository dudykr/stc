#!/usr/bin/env bash
set -eu

function sortFile {
    mkdir -p .stc
    cat $1 | awk NF | sort | uniq | awk '{$1=$1};1' | uniq | sort > .stc/tmp.txt
    mv .stc/tmp.txt $1
}

GIST_DIR="$HOME/projects/stc-issue-split"
echo "Gist dir: $GIST_DIR"

# Clone git@github.com:dudykr/stc-issue-split.git to your CDPATH
(cd $GIST_DIR && git pull || true)

echo "Sorting done.txt"
(cd $GIST_DIR && sortFile done.txt)

find crates/* -name "*.stats.rust-debug" > "$GIST_DIR/.stc/all.txt"

echo "comm"
comm -23 "$GIST_DIR/.stc/all.txt" "$GIST_DIR/done.txt" > .stc/dedup.txt

cat .stc/dedup.txt \
    | xargs grep 'extra_error: [1-9][0-9]*' \
    | sort -n -k 3 -t ":" -r \
    > "$GIST_DIR/list.txt"