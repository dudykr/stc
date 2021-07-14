var a14: (x: { a: string; b: number }) => void;
var b14: <T>(x: { a: T; b: T }) => void;
a14 = b14; // ok

export { }