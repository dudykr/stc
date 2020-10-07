// @strictNullChecks: true
// @noimplicitany: true
// @declaration: true

type Box<T> = {
    value: T;
}

type Boxified<T> = {
    [P in keyof T]: Box<T[P]>;
}

declare function box<T>(x: T): Box<T>;

function makeRecord<T, K extends string>(obj: { [P in K]: T }) {
    return obj;
}

let b = makeRecord({
    a: box(42),
    b: box("hello"),
    c: box(true)
});