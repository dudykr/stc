class Bar {
    a: number;
    b?: number;
    c?= 2;

    constructor(public d?: number, public e = 10) {
    }

    f() {
        return 1;
    }

    g?(): number;  // Body of optional method can be omitted
    h?() {
        return 2;
    }
}

declare var x: Bar;
const a1 = x.a;
const a2 = x.b;
const a3 = x.c;
const a4 = x.d;
const a5 = x.e;
const a6 = x.f;
const a7 = x.g;