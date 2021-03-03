class A {
    propA: number;
}

class B {
    propB: number;
}

class C extends A {
    propC: number;
}

let union2: C | B;
let union3: boolean | B = isA(union2) || union2;

export { }