declare function f24<T, U, K extends keyof T | keyof U>(obj: Pick<T & U, K>): T & U;

let x4 = f24({ foo: 42, bar: "hello" });
