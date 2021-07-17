class Base { foo: string; }
class Derived extends Base { bar: string; }

var a6: <T extends Base>(arg: T) => Derived
var b6: <T extends Base, U extends Derived>(arg: T) => U
b6 = a6; // ok
a6 = b6;


export { }