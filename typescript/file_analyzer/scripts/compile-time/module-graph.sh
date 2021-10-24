#!/usr/bin/env bash
set -eu

cargo modules generate graph --package stc_ts_file_analyzer > module-graph.dot