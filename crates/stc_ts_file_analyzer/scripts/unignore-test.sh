#!/usr/bin/env bash
set -eu
echo "$@"
mv "$1" "$(echo $1 | sed 's!/\.!/!')"