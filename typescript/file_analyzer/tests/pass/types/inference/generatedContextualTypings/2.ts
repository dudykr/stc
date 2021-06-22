class Base { private p; }
class Derived1 extends Base { private m; }
class Derived2 extends Base { private n; }

var b = new Base(), d1 = new Derived1(), d2 = new Derived2();
interface Genric<T> { func(n: T[]); }


export class x95 { constructor(parm: (s: Base[]) => any = n => { var n: Base[]; return null; }) { } }
export class x96 { constructor(parm: Genric<Base> = { func: n => { return [d1, d2]; } }) { } }
