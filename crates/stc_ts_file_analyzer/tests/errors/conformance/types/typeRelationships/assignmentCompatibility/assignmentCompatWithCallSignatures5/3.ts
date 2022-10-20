// checking assignment compat for function types. No errors in this file

class Base { foo: string; }

var a15: <T>(x: { a: T; b: T }) => T[];


var b15: <U, V>(x: { a: U; b: V; }) => U[];
b15 = a15; // ok

export { }