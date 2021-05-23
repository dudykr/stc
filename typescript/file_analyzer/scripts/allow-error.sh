#!/usr/bin/env bash
set -eu

NEW_FILE=$(echo "$1" | sed 's!tests/pass!tests/visualize!')

mv $1 $NEW_FILE