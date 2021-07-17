class Base { foo: string; }
class Derived extends Base { bar: string; }
class Derived2 extends Derived { baz: string; }

var a12: (x: Array<Base>, y: Array<Derived2>) => Array<Derived>;
var b12: (x: Array<Base>, y: Array<Base>) => Array<Derived2>;
a12 = b12;

export { }