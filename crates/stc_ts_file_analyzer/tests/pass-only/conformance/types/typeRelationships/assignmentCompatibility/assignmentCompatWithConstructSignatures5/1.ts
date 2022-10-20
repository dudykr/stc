// checking assignment compat for function types. All valid

class Base { foo: string; }
class Derived extends Base { bar: string; }

var a17: {
    new <T extends Derived>(x: new (a: T) => T): T[];
    new <T extends Base>(x: new (a: T) => T): T[];
};

var b17: new <T>(x: new (a: T) => T) => T[];
a17 = b17; // ok
b17 = a17; // ok

export { }