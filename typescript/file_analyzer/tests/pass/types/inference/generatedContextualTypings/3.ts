class Base { private p; }
class Derived1 extends Base { private m; }
class Derived2 extends Base { private n; }

var b = new Base(), d1 = new Derived1(), d2 = new Derived2();

export function x123(parm: () => Base[] = function named() { return [d1, d2] }) { }
export function x126(parm: { (): Base[]; } = function named() { return [d1, d2] }) { }
