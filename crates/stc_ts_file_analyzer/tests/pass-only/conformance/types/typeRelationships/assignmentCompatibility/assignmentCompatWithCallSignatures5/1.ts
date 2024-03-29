// checking assignment compat for function types. No errors in this file

class Base { foo: string; }
class Derived extends Base { bar: string; }

var a6: <T extends Base>(x: (arg: T) => Derived) => T;

var b6: <T extends Base, U extends Derived>(x: (arg: T) => U) => T;
b6 = a6; // ok
a6 = b6; // ok


export { }