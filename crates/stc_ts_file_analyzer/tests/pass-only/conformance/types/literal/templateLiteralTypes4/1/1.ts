
// @strict: true
// @target: esnext
// @declaration: true

// Use constrained `infer` in template literal to get ordinal indices as numbers:
type IndexFor<S extends string> = S extends `${infer N extends number}` ? N : never;
type IndicesOf = IndexFor<Extract<keyof TDef, string>>; // ordinal indices as number literals

declare var a: IndicesOf
declare var a: 0 | 1

type TDef = [
    { name: "x", type: "f64" },
    { name: "y", type: "f64" },
]

export { a }
