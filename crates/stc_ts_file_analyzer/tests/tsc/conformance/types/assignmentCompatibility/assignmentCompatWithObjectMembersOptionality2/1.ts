// M is optional and S contains no property with the same name as M
// N is optional and T contains no property with the same name as N

class Base { foo: string; }

// targets
interface C {
    opt?: Base
}
var c: C;


// sources
interface D {
    other: Base;
}


var d: D;

// disallowed by weak type checking
c = d;


export { }