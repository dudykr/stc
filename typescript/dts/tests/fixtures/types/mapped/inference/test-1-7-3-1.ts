declare function f20<T, K extends keyof T>(obj: Pick<T, K>): T;

let x0 = f20({ foo: 42, bar: "hello" });
