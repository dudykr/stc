// Derived type indexer must be subtype of base type indexer

interface Base { foo: string; }
interface Derived extends Base { bar: string; }

class A {
    [x: string]: Derived;
}

export class B extends A {
    [x: string]: Base; // error
}

