#!/usr/bin/env bash
set -eu

function sortFile {
    mkdir -p .stc
    cat $1 | awk NF | sort | uniq | awk '{$1=$1};1' | uniq | sort > .stc/tmp.txt
    mv .stc/tmp.txt $1
}

GIST_DIR="$(cd stc-issue-split && pwd)"
echo "Gist dir: $GIST_DIR"

# Clone git@github.com:dudykr/stc-issue-split.git to your CDPATH
(cd $GIST_DIR && git pull || true)

echo "Sorting done.txt"
(cd $GIST_DIR && sortFile done.txt)

echo "Copying done.txt"
cp $GIST_DIR/done.txt .stc

echo "Find"
find crates/* -name "*.stats.rust-debug" > .stc/all-tests.txt

echo "comm"
comm -23 .stc/all-tests.txt .stc/done.txt > .stc/tests-to-split.txt

cat .stc/tests-to-split.txt | \
    | xargs grep 'extra_error: [1-9][0-9]*' \
    | sort -n -k 3 -t ":" -r \
    > .stc/all-tests-to-split.txt