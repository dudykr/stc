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

var cnd: C & D;
var y: (A | B) & (C | D);

y = cnd;

export { }