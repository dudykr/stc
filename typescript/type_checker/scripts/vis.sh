#!/usr/bin/env bash
set -eu

err_handler () {
    ./scripts/_/notify.sh 'Visualization test failed!'
    exit 1
}

trap err_handler ERR

cd ../file_analyzer
scripts/visualize.sh $@