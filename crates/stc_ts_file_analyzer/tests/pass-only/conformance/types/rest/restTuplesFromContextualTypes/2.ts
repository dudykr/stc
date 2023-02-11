// @strict: true
// @declaration: true

declare const t2: [number, boolean, ...string[]];

declare function f2(cb: (...args: typeof t2) => void): void;

f2((...x) => { })

export { }