// checking assignment compat for function types. All valid

class Base { foo: string; }
class Derived extends Base { bar: string; }
class Derived2 extends Derived { baz: string; }

var a18: {
    new(x: {
        new <T extends Derived>(a: T): T;
        new <T extends Base>(a: T): T;
    }): any[];
    new(x: {
        new <T extends Derived2>(a: T): T;
        new <T extends Base>(a: T): T;
    }): any[];
};

var b18: new (x: new <T>(a: T) => T) => any[];
a18 = b18; // ok
b18 = a18; // ok

export { }