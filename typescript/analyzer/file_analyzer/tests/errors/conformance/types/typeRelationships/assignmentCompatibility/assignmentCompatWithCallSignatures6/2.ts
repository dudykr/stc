// checking assignment compatibility relations for function types. All valid

class Base { foo: string; }

interface A {
    a16: <T extends Base>(x: { a: T; b: T }) => T[];
}

var x: A;

var b16: <T>(x: { a: T; b: T }) => T[];
b16 = x.a16;

export { }