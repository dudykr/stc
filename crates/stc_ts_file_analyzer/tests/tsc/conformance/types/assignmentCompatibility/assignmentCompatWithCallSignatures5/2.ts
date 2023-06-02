// checking assignment compat for function types. No errors in this file

class Base { foo: string; }

var a11: <T>(x: { foo: T }, y: { foo: T; bar: T }) => Base;

var b11: <T, U>(x: { foo: T }, y: { foo: U; bar: U }) => Base;
b11 = a11; // ok

export { }