// TypeScript Spec, section 4.12.2:
// If e is an expression of a function type that contains exactly one generic call signature and no other members,
// and T is a function type with exactly one non - generic call signature and no other members, then any inferences
// made for type parameters referenced by the parameters of T's call signature are fixed, and e's type is changed
// to a function type with e's call signature instantiated in the context of T's call signature (section 3.8.5).

declare function bar<T, U, V>(x: T, y: U, cb: (x: T, y: U) => V): V;

declare function h<T, U>(x: T, y: U): T[] | U[];

var d: number[] | string[];
var d = bar(1, "one", h);  // Should be number[] | string[]
var d = bar("one", 1, h);  // Should be number[] | string[]

export { d }