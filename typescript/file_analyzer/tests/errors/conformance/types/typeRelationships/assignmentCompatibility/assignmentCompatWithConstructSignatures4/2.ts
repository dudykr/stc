
class Base { foo: string; }
class Derived extends Base { bar: string; }
class Derived2 extends Derived { baz: string; }

// target type with non-generic call signatures
var a17: {
    new(x: {
        new <T extends Derived>(a: T): T;
        new <T extends Base>(a: T): T;
    }): any[];
    new(x: {
        new <T extends Derived2>(a: T): T;
        new <T extends Base>(a: T): T;
    }): any[];
};


var b17: new <T>(x: (a: T) => T) => any[];
a17 = b17; // error
b17 = a17; // error



export { }