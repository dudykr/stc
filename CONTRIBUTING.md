# Contributing to stc

## New, faster workflow

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

### Running one test

You can invoke single conformance test case by using `./scripts/test.sh` from `./crates/stc_ts_type_checker`.

```bash
./scripts/test.sh staticIndexSignature
```

will run conformance tests which have `staticIndexSignature` in their test name.
You can use filename, too and with it you can run single test.

## Directories for testing

See [the readme of the file analyzer](./crates/stc_ts_file_analyzer/) for more details.

## Debugging tips

### Debugging with VScode Debugger

Please copy `.vscode/launch.template.json` and rename it to
`.vscode/launch.json`.

In `launch.json`, you can pass the path of the `.ts` file you want to check to the
`args` field. Then, you can run the type checker with debugger from the
debugger panel.

### `print_backtrace()`

If you think some code should not be invoked but seems like it's invoked, you can use `print_backtrace()`.
It will automatically exclude useless items from the stack trace.

### Matching over `Type`

To avoid performance drop caused by cloning, stc uses `Type::Arc` to share memory.
But there's a downside for this - you have to call `normalize()` while matching.

e.g.

```rust
match ty.normalize() {
}
```

Note that this is different from `self.normalize(ty)` of `Analyzer`, which is used to expand special types like references, queries, mapped, etc..

### Finding problematic code

Look at the `context:` in the error output, and search it from repository using substring of the context.
e.g. `tried to assign to a template type` means the error comes from https://github.com/dudykr/stc/blob/1c73ce5fa87ec789ead108c5c6876e8089b1e5de/crates/stc_ts_file_analyzer/src/analyzer/assign/mod.rs#L2208

## Updating performance stats

You can run `./scripts/release-time.sh` from `./crates/stc_ts_type_checker` to update timing stats file.
