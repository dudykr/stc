class Base { foo: string; }
class Derived extends Base { bar: string; }
class Derived2 extends Derived { baz: string; }

var a12: (x: Base, y: Derived2) => Derived;
var b12: (x: Base, y: Base) => Derived2;
a12 = b12;

export { }