// @strict: true
// @declaration: true

declare const t1: [number, boolean, string];

declare function f1(cb: (...args: typeof t1) => void): void;

f1((a, b, c) => { })
f1((...x) => { })

export { }