// @strictNullChecks: true
// @noimplicitany: true
// @declaration: true

type Box<V> = {
    value: V;
}

type Boxified<R> = {
    [P in keyof R]: Box<R[P]>;
}

declare function unboxify<T>(obj: Boxified<T>): T;

declare let b: {
    a: Box<string>,
};
let v = unboxify(b);