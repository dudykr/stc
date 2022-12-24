function foo2<T extends "foo" | "bar">(x: Uppercase<T>) {
    let s: "FOO" | "BAR" = x;
}
