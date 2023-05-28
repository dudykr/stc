// checking subtype relations for function types as it relates to contextual signature instantiation
// same as subtypingWithConstructSignatures4 but using class type parameters instead of generic signatures
// all are errors

class Base { foo: string; }
class Derived extends Base { bar: string; }

interface A { // T
    // M's
    a4: new <T, U>(x: T, y: U) => string;
}

export interface I4<T> extends A {
    a4: new <U>(x: T, y: U) => string;
}



