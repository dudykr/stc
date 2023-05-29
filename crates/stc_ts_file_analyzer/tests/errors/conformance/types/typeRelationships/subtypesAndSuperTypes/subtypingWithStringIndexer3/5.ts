// Derived type indexer must be subtype of base type indexer

interface Base { foo: string; }
interface Derived extends Base { bar: string; }
interface Derived2 extends Derived { baz: string; }




export module Generics {
    class A<T extends Derived> {
        [x: string]: T;
    }





    export class B5<T extends Derived2> extends A<T> {
        [x: string]: Derived2; // error
    }
}
