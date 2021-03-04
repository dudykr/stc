class A {
    propA: number;
}

class B {
    propB: number;
}

class C extends A {
    propC: number;
}

declare function isA(p1: any): p1 is A;


let union2: C | B;
let union3: boolean | B = isA(union2) || union2;

export { }