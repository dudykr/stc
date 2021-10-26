class A {
    propA: number;
}

class C extends A {
    propC: number;
}

declare function isA(p1: any): p1 is A;

var subType: C;
if (isA(subType)) {
    subType.propC;
}

export { }