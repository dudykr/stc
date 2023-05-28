// checking subtype relations for function types as it relates to contextual signature instantiation
// same as subtypingWithConstructSignatures4 but using class type parameters instead of generic signatures
// all are errors

class Base { foo: string; }
class Derived extends Base { bar: string; }

interface A { // T
    // M's
    a5: new <T, U>(x: (arg: T) => U) => T;
}


export interface I5<T> extends A {
    a5: new <U>(x: (arg: T) => U) => T;
}


