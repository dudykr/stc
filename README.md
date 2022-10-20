# stc

TypeScript type checker written in Rust.

## Contributing

### Setup

A `yarn` invocation will install required dependencies.

### Running tests

You can run `./scripts/test.sh` from `./crates/stc_ts_type_checker` to run tests.

e.g. `./scripts/test.sh union` will run tests in passing list and tests containing `union` in its name.

This will update passing test count, so you can see if your fix is correct by checking numbers in `tests/tsc-stats.rust-debug`.

### Updating passing test list

You can update the passing test list by invoking `./scripts/check.sh` from `./typescript/stc_ts_type_checker`.

### Updating performance stats

You can run `./scripts/release-time.sh` from `./typescript/stc_ts_type_checker` to update timing stats file.
