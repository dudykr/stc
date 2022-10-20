enum Kind {
    A,
    B,
}

interface Base {
    kind: Kind;
}

interface A extends Base {
    kind: Kind.A;
    yar: any;
}

interface B extends Base {
    kind: Kind.B;
    gar: any;
}

type Both = A | B;
function isBoth(x: Base): x is Both {
    return true;
}

let foo: Base = undefined;
if (isBoth(foo)) {
    switch (foo.kind) {
        case Kind.A:
            const myA: A = foo; // Should not be an error
            break;
        case Kind.B:
            const myB: B = foo;
            break;
    }
}
