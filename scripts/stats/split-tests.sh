#!/usr/bin/env bash
set -eu

function sortFile {
    mkdir -p .stc
    cat $1 | awk NF | sort | uniq | awk '{$1=$1};1' | uniq | sort > .stc/tmp.txt
    mv .stc/tmp.txt $1
}

git pull || true

# Clone git@github.com:dudykr/stc-issue-split.git to ~/projects

GIST_DIR="$HOME/projects/8198130f16e42514b22656c57690b124"
echo "Gist dir: $GIST_DIR"
(cd $GIST_DIR && git pull || true)

echo "Sorting done.txt"
(cd $GIST_DIR && sortFile done.txt)
echo "Sorting issue-required.txt"
(cd $GIST_DIR && sortFile issue-required.txt)

find crates/* -name "*.stats.rust-debug" | xargs grep -l 'extra_error: [1-9][0-9]*' > "$GIST_DIR/.stc/all.txt"
sortFile "$GIST_DIR/.stc/all.txt"

comm -23 "$GIST_DIR/done.txt" "$GIST_DIR/.stc/all.txt" > "$GIST_DIR/.stc/tmp.txt"
comm -23 "$GIST_DIR/done.txt" "$GIST_DIR/.stc/tmp.txt" > "$GIST_DIR/.stc/tmp2.txt"
mv "$GIST_DIR/.stc/tmp2.txt" "$GIST_DIR/done.txt"

echo "comm (1)"
comm -23 "$GIST_DIR/.stc/all.txt" "$GIST_DIR/done.txt" > "$GIST_DIR/.stc/dedup.txt"

echo "comm (2)"
sortFile "$GIST_DIR/.stc/dedup.txt"
comm -23 "$GIST_DIR/.stc/dedup.txt" "$GIST_DIR/issue-required.txt" > "$GIST_DIR/.stc/dedup2.txt"

cat "$GIST_DIR/.stc/dedup2.txt" \
    | xargs grep 'extra_error: [1-9][0-9]*' \
    | sort -n -k 3 -t ":" -r \
    > "$GIST_DIR/list.txt"

(cd $GIST_DIR && git commit -am "Update" && git push)