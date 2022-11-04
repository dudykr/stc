declare abstract class A {
    constructor(p: number);
    a: number;
}

declare function B(p: string): B;
declare class B extends A {
    constructor(p: string);
    b: number;
}

let b = new B("Hey")
console.log(b.a)