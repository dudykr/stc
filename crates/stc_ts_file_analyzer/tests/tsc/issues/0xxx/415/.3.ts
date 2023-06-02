// @strict: true 

interface A {
    a: string;
}
interface B {
    b: string;
}
interface C {
    c: string;
}
interface D {
    d: string;
}

var cod: C | D;
var x: (A & B) | (C & D);

cod = x;

export { }