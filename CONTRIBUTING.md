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
Look at it, and if it's wrong, adjust the Rust source code, and run `./scripts/check.sh` to verify if your fix is correct.
If you are done, please file a pull request on GitHub.

### Updating performance stats

You can run `./scripts/release-time.sh` from `./crates/stc_ts_type_checker` to update timing stats file.

## Troubleshooting

1. In case if you're using Windows and getting an error

```sh
error: `-Csplit-debuginfo=unpacked` is unstable on this platform
```

as a workaround you can try using [WSL](https://learn.microsoft.com/en-us/windows/wsl/).

2. If you're getting something like

```sh
--- stderr
  thread 'main' panicked at 'failed to execute command: No such file or directory (os error 2)', /home/ubuntu/.cargo/registry/src/github.com-1ecc6299db9ec823/tikv-jemalloc-sys-0.5.2+5.3.0-patched/build.rs:326:19
```

try running `apt-get install make`. ([related issue](https://github.com/gnzlbg/jemallocator/issues/148#issuecomment-619373613))

3. If you are getting a weird error after modifying something, try deleting cache.

To speed up development, [PR #148](https://github.com/dudykr/stc/pull/148) introduced caching of builtin types.
It stores cache at `./crates/stc_ts_file_analyzer/.stc` and at `./crates/stc_ts_type_checker/.stc`, respectively for tests of each crates.
