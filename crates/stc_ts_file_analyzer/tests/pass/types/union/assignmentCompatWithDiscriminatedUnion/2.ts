// see 'typeRelatedToDiscriminatedType' in checker.ts:

// Dropping constituents of T
namespace Example2 {
    type S = { a: 0 | 2, b: 4 };
    type T = { a: 0, b: 1 | 4 }     // T0
        | { a: 1, b: 2 }         // T1
        | { a: 2, b: 3 | 4 };    // T2
    declare let s: S;
    declare let t: T;

    // S is assignable to T0 when S["a"] is 0
    // S is assignable to T2 when S["a"] is 2
    t = s;
}

