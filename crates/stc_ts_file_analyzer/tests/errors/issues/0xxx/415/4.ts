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

var aob: A | B;
var x: (A & B) | (C & D);

aob = x;

export { }