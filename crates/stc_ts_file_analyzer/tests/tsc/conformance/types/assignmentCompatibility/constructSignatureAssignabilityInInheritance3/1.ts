// checking subtype relations for function types as it relates to contextual signature instantiation
// error cases

class Base { foo: string; }
class Derived extends Base { bar: string; }
class Derived2 extends Derived { baz: string; }


// base type with non-generic call signatures
interface A {
    a2: new (x: number) => string[];
}


export interface I2<T, U> extends A {
    a2: new (x: T) => U[]; // error, no contextual signature instantiation since I2.a2 is not generic
}
