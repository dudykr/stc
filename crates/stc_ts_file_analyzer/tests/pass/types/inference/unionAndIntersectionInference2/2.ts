declare function f1<T>(x: T | string): T;

var d1: string | { name: string };
f1(d1); // { name: string }