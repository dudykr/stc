
// @strict: true
// @target: esnext
// @declaration: true

// Use constrained `infer` in template literal to get ordinal indices as numbers:
type IndexFor<S extends string> = S extends `${infer N extends number}` ? N : never;
type IndicesOf = IndexFor<Extract<keyof TDef, string>>; // ordinal indices as number literals

interface FieldDefinition {
    readonly name: string;
    readonly type: "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "f32" | "f64";
}

type FieldType<T extends FieldDefinition["type"]> =
    T extends "i8" | "i16" | "i32" | "u8" | "u16" | "u32" | "f32" | "f64" ? number :
    T extends "f32" | "f64" ? bigint :
    never;

// Default members
interface TypedObjectMembers {
    // get/set a field by index
    getIndex<I extends IndicesOf>(index: I): FieldType<Extract<TDef[I], FieldDefinition>["type"]>;
}

type TypedObject =
    & TypedObjectMembers;

type TDef = [
    { name: "x", type: "f64" },
    { name: "y", type: "f64" },
]
// NOTE: type would normally be created from something like `const Point = TypedObject([...])` from which we would infer the type
type Point = TypedObject;

declare const p: Point;
p.getIndex(0); // ok, 0 is a valid index

export { }
