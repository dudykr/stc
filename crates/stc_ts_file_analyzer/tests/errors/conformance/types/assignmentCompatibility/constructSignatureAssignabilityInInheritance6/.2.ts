// checking subtype relations for function types as it relates to contextual signature instantiation
// same as subtypingWithConstructSignatures4 but using class type parameters instead of generic signatures
// all are errors

class Base { foo: string; }
class Derived extends Base { bar: string; }

interface A { // T
    // M's
    a5: new <C, D>(x: (arg: C) => D) => C;
}


export interface I5<E> extends A {
    a5: new <F>(x: (arg: E) => F) => E;
}


