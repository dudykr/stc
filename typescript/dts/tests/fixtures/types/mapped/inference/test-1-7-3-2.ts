declare function f21<T, K extends keyof T>(obj: Pick<T, K>): K;

let x1 = f21({ foo: 42, bar: "hello" });
