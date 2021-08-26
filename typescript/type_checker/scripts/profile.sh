#!/usr/bin/env bash

set -eux

docker run -d -p6831:6831/udp -p6832:6832/udp -p16686:16686 jaegertracing/all-in-one:latest || true

cargo test --release --test tsc