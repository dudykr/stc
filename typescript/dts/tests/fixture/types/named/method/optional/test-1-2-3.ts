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
let f1 = x.f();
let g1 = x.g && x.g();
let g2 = x.g ? x.g() : 0;
let h1 = x.h && x.h();
let h2 = x.h ? x.h() : 0;