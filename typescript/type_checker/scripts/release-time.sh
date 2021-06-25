#!/usr/bin/env bash

set -eux

TEST='' cargo test --test tsc --release 2>&1 > /dev/null