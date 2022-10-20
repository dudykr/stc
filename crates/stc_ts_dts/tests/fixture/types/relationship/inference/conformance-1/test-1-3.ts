// @strict: true
// @declaration: true

declare function f3<T>(cb: <S extends Array<S>>(x: S) => T): T;

let x3 = f3(x => x);  // Array<any>

