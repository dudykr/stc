declare function f1<T>(x: T | string): T;

var b1: string | string[];
f1(b1); // string[]