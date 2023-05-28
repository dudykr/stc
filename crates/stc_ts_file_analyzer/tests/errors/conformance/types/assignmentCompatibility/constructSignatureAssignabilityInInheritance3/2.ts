// checking subtype relations for function types as it relates to contextual signature instantiation
// error cases

class Base { foo: string; }
class Derived extends Base { bar: string; }
class Derived2 extends Derived { baz: string; }
class OtherDerived extends Base { bing: string; }

// base type has generic call signature
interface B {
    a2: new <T>(x: T) => T[];
}

// base type has generic call signature
interface C {
    a2: new <T>(x: T) => string[];
}

export interface I7 extends C {
    a2: new <T>(x: T) => T[]; // error
}

export { }