// Derived member is not optional but base member is, should be ok

class Base { foo: string; }
class Derived extends Base { bar: string; }

// targets
interface C {
    opt: Base
}
var c: C;

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

c = d; // error
c = e; // error
export { }