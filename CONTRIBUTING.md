# Contributing to stc

## Configuring development environment

1. Clone stc.
2. Run `yarn` in the cloned directory.

## Typical workflow

Basically all work starts from `./crates/stc_ts_type_checker`.

### Fixing type inference

**Note: This is what we are focusing on, and want help for.**

As this part is the most important task for now, this is the first item in this document.

First, you should find a erroneous test case.
Typically, you can find one by running `./scripts/check.sh` from `./crates/stc_ts_type_checker`.
It will print enormous amount of log.
You should focus on false-positives.
Each error will contain lots of information required for debugging, with `context:` prefix.
Read it, and if it's a wrong error, copy in to `./crates/stc_ts_file_analyzer/tests/pass`.
You should reducing the file to minimal repro, by removing unrelated lines.
Typically we use `foo/1.ts` for `foo.ts` from a conformance test suite.

After then, run `./scripts/check.sh` again.
It will run the only test you added.
It will dump type information to a `foo.swc-stderr` file.
Look at it, and if it's wrong
