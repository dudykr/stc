# Common pitfalls

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
