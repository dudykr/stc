type A = readonly string[] extends unknown[] ? string : never;

let a: A = "1" as never;
a; // never;

type B = string[] extends readonly unknown[] ? string : never;
let b: B = "1" as string;
b; // string

type C = readonly string[] extends readonly unknown[] ? string : never;
let c: C = "1" as string;
c; // string
