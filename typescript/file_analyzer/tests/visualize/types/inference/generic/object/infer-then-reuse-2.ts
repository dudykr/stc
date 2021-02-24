
enum E1 { X }
enum E2 { X }

// Check that we infer from both a.r and b before fixing T in a.w

declare function f1<T, U>(a: { w: (x: T) => U; r: () => T; }, b: T): U;

var v1: number;
var v1 = f1({ w: x => x, r: () => 0 }, 0);
var v1 = f1({ w: x => x, r: () => 0 }, E1.X);
var v1 = f1({ w: x => x, r: () => E1.X }, 0);

var v2: E1;
var v2 = f1({ w: x => x, r: () => E1.X }, E1.X);

var v3 = f1({ w: x => x, r: () => E1.X }, E2.X);  // Error

export { }