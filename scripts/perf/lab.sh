#!/usr/bin/env bash
set -eu

cargo install --path .

export LD_LIBRARY_PATH="$(rustc --print sysroot)/lib"

(cd stc-lab/rxjs && stc tsc)