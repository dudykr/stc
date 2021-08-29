#!/usr/bin/env bash
set -eu

cargo install --path .

(cd stc-lab/rxjs && stc)