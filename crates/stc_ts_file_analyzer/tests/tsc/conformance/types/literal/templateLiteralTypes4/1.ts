// @strict: true
// @target: esnext
// @declaration: true

// example use case (based on old TypedObjects proposal):

// Use constrained `infer` in template literal to get ordinal indices as numbers:
type IndexFor<S extends string> = S extends `${infer N extends number}` ? N : never;
type IndicesOf<T> = IndexFor<Extract<keyof T, string>>; // ordinal indices as number literals

interface FieldDefinition {
    readonly name: string;
    readonly type: "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "f32" | "f64";
}

type FieldType<T extends FieldDefinition["type"]> =
    T extends "i8" | "i16" | "i32" | "u8" | "u16" | "u32" | "f32" | "f64" ? number :
    T extends "f32" | "f64" ? bigint :
    never;

// Generates named members like `{ x: number, y: bigint }` from `[{ name: "x", type: "i32" }, { name: "y", type: "i64" }]`
type TypedObjectNamedMembers<TDef extends readonly FieldDefinition[]> = {
    [P in TDef[number]["name"]]: FieldType<Extract<TDef[number], { readonly name: P }>["type"]>;
};

// Generates ordinal members like `{ 0: number, 1: bigint }` from `[{ name: "x", type: "i32" }, { name: "y", type: "i64" }]`
type TypedObjectOrdinalMembers<TDef extends readonly FieldDefinition[]> = {
    [I in Extract<keyof TDef, `${number}`>]: FieldType<Extract<TDef[I], FieldDefinition>["type"]>;
};

// Default members
interface TypedObjectMembers<TDef extends readonly FieldDefinition[]> {
    // get/set a field by name
    get<K extends TDef[number]["name"]>(key: K): FieldType<Extract<TDef[number], { readonly name: K }>["type"]>;
    set<K extends TDef[number]["name"]>(key: K, value: FieldType<Extract<TDef[number], { readonly name: K }>["type"]>): void;

    // get/set a field by index
    getIndex<I extends IndicesOf<TDef>>(index: I): FieldType<Extract<TDef[I], FieldDefinition>["type"]>;
    setIndex<I extends IndicesOf<TDef>>(index: I, value: FieldType<Extract<TDef[I], FieldDefinition>["type"]>): void;
}

type TypedObject<TDef extends readonly FieldDefinition[]> =
    & TypedObjectMembers<TDef>
    & TypedObjectNamedMembers<TDef>
    & TypedObjectOrdinalMembers<TDef>;

// NOTE: type would normally be created from something like `const Point = TypedObject([...])` from which we would infer the type
type Point = TypedObject<[
    { name: "x", type: "f64" },
    { name: "y", type: "f64" },
]>;

declare const p: Point;
p.getIndex(2); // error, 2 is not a valid index


export { }