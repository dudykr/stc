// Derived member is not optional but base member is, should be ok

class Base { foo: string; }
class Derived extends Base { bar: string; }

// targets
interface C {
    opt: Base
}
var c: C;

var a: { opt: Base; }
var b = { opt: new Base() }

// sources
interface D {
    opt?: Base;
}
interface E {
    opt?: Derived;
}
interface F {
    opt: Derived;
}
var d: D;
var e: E;
var f: F;

c = d; // error
c = e; // error

a = d; // error
a = e; // error

b = d; // error
b = e; // error

export { }