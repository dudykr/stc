#!/usr/bin/env bash
set -eu

# Usage: ./scripts/errors/find-extra.sh {ts error code}
# 
# ./scripts/errors/find-extra.sh TS2322
#

while IFS= read -r line; do
    # echo "... $line ..."
    count=`jq -r "(.extra_errors.$1 // 0)" "$line"`

    if [ "$count" -ne "0" ]; then
        echo "File $line has $count extra errors of type $1";
    fi
done <<< "$(./scripts/errors/list-files.sh)"