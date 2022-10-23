// see 'typeRelatedToDiscriminatedType' in checker.ts:

// IteratorResult
namespace Example1 {
    type S = { done: boolean, value: number };
    type T =
        | { done: true, value: number }     // T0
        | { done: false, value: number };   // T1

    declare let s: S;
    declare let t: T;

    // S is assignable to T0 when S["done"] is true
    // S is assignable to T1 when S["done"] is false
    t = s;
}
