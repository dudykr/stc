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

var anb: A & B;
var y: (A | B) & (C | D);

y = anb;

export { }