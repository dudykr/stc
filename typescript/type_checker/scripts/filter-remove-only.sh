#!/usr/bin/env bash

set -eu


TEST='' cargo test --test tsc \
    | grep '\[REMOVE_ONLY\]'