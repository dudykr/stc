// M is optional and S contains no property with the same name as M
// N is optional and T contains no property with the same name as N

class Base { foo: string; }
class Derived extends Base { bar: string; }


var a: { opt?: Base; }
var b: typeof a = { opt: new Base() }

interface E {
    other: Derived;
}
var e: E;

// disallowed by weak type checking
b = e;

export { }