declare function f23<T, U extends keyof T, K extends U>(obj: Pick<T, K>): T;

let x3 = f23({ foo: 42, bar: "hello" });
