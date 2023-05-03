function someGenerics7<A, B, C>(
    a: (a: A) => A,
    b: (b: B) => B,
    c: (c: C) => C
) { }

function someGenerics8<T>(n: T): T {
    return n;
}

var x = someGenerics8(someGenerics7); // return type should be inferred as someGenerics7, as T -> someGenerics7

// stc infers x as (a: (a: unknown) => unknown, b: (b: unknown) => unknown, c: (c: unknown) => unknown) => void; (AFAICT)
// but it should be <A#3#0, B#3#0, C#3#0>(a: (a: A#3) => A, b: (b: B#3) => B, c: (c: C#3) => C) => void

x<string>(null, null, null); // No error from stc, should be 2558 Expected 3 arguments but got 1

export { }