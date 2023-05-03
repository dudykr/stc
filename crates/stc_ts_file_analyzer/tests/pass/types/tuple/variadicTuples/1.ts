// @strict: true
// @declaration: true

// Variadics in tuple types

type First<T extends readonly unknown[]> = T extends readonly [
  unknown,
  ...unknown[]
]
  ? T[0]
  : T[0] | undefined;

let R00 = 1 as First<readonly [number, symbol, string]>;
let R01 = {} as First<readonly [symbol, string]>;
let R02 = {} as First<readonly [string]>;
let R03 = {} as First<readonly [number, symbol, ...string[]]>;
let R04 = {} as First<readonly [symbol, ...string[]]>;
let R05 = {} as First<readonly string[]>;
let R06 = undefined as First<readonly []>;
