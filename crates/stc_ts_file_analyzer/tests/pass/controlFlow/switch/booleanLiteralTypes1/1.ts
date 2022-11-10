
function assertNever(x: never): never {
    throw new Error("Unexpected value");
}

type Item =
    { kind: true, a: string } |
    { kind: false, b: string };

function f21(x: Item) {
    switch (x.kind) {
        case true: return x.a;
        case false: return x.b;
    }
    return assertNever(x);
}

export { }