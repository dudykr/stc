#!/usr/bin/env bash
set -eu

# Start jaeger
docker run -d -p6831:6831/udp -p6832:6832/udp -p16686:16686 jaegertracing/all-in-one:latest || true



export RUST_LOG=trace,swc_common=off

cargo test --test perf --release --features profile $@ -- --nocapture