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
var cod: C | D;
var anb: A & B;
var cnd: C & D;
var x: (A & B) | (C & D);
var y: (A | B) & (C | D);

y = cnd;

export { }