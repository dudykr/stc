// checking subtype relations for function types as it relates to contextual signature instantiation
// error cases

class Base { foo: string; }
class Derived extends Base { bar: string; }
class Derived2 extends Derived { baz: string; }


// base type with non-generic call signatures
interface A {
    a7: new (x: (arg: Base) => Derived) => (r: Base) => Derived2;
}


export interface I3 extends A {
    // valid, no inferences for V so it defaults to Derived2
    a7: new <T extends Base, U extends Derived, V extends Derived2>(x: (arg: T) => U) => (r: T) => V;
}

