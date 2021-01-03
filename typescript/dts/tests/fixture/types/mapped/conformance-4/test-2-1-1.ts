type Box<T> = {
    value: T
};

type Boxified<T> = {
    [P in keyof T]: Box<T[P]>;
};

type Tester<A, B> = {
    tag: A,
    boxified: Boxified<B>,
}

declare function tester<T, U>(t: T, u: U): Tester<T, U>

type A = { a: string };
type B = { b: string };
type C = { c: string };

function f1(x: A | B | C | undefined) {
    return tester('string', x);
}