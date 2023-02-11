//@strict: true

interface Covariant<T> {
    foo: T extends string ? T : number;
}

interface Contravariant<T> {
    foo: T extends string ? keyof T : number;
}

interface Invariant<T> {
    foo: T extends string ? keyof T : T;
}

export function f2<A, B extends A>(a: Contravariant<A>, b: Contravariant<B>) {
    b = a;
}
