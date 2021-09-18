// checking assignment compat for function types. No errors in this file

var a3: <T>(x: T) => void;

var b3: <T>(x: T) => T;
b3 = a3; // ok
export { }