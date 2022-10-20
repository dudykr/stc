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
