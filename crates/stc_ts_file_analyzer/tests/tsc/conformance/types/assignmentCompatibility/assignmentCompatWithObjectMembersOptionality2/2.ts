// M is optional and S contains no property with the same name as M
// N is optional and T contains no property with the same name as N

class Base { foo: string; }
class Derived extends Base { bar: string; }

// targets
interface C {
    opt?: Base
}
var c: C;


interface E {
    other: Derived;
}
var e: E;

// disallowed by weak type checking
c = e;

export { }