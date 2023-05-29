// Derived type indexer must be subtype of base type indexer

interface Base { foo: string; }
interface Derived extends Base { bar: string; }




export module Generics {
    class A<T extends Derived> {
        [x: string]: T;
    }

    export class B extends A<Base> {
        [x: string]: Derived; // error
    }

}
