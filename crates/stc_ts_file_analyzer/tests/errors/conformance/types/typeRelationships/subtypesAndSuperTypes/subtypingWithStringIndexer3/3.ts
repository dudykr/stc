// Derived type indexer must be subtype of base type indexer

interface Base { foo: string; }
interface Derived extends Base { bar: string; }




export module Generics {
    class A<T extends Derived> {
        [x: string]: T;
    }



    export class B3<T extends Derived> extends A<T> {
        [x: string]: Base; // error
    }


}
