

class C1 {
    p1: string;
}
class C2 {
    p2: number;
}
class D1 extends C1 {
    p3: number;
}
class C3 {
    p4: number;
}

var ctor6: C1 | C2 | C3;
if (ctor6 instanceof C1 || ctor6 instanceof C2) {
}
else {
    ctor6.p4; // C3
}

export { }