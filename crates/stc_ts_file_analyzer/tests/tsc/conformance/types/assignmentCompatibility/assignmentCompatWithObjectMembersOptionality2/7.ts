// M is optional and S contains no property with the same name as M
// N is optional and T contains no property with the same name as N

class Base { foo: string; }


var a: { opt?: Base; }
var b: typeof a = { opt: new Base() }

// sources
interface D {
    other: Base;
}
var d: D;

// disallowed by weak type checking
b = d;

export { }