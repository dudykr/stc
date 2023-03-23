// @target: ES5

var x: { foo: string; }

class Base {
    a: typeof x;
    b(a: typeof x) { }
    get c() { return x; }
    set c(v: typeof x) { }
    d: (a: typeof x) => void;

    static r: typeof x;
    static s() { }
    static get t() { return x; }
    static set t(v: typeof x) { }
    static u: (a: typeof x) => void;

    constructor(a: typeof x) { }
}

// Errors
// decrease visibility of all public members to protected



export class Derived4 extends Base {
    protected set c(v: typeof x) { }
    constructor(a: typeof x) { super(a); }
}






