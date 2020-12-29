// @strictNullChecks: true
// @declaration: true


declare function mapObject<K extends string, T8, U>(obj: Record<K, T8>, f: (x: T8) => U): Record<K, U>;

const rec1 = { foo: "hello", bar: "world", baz: "bye" };
const lengths1 = mapObject(rec1, s => s.length);  // { foo: number, bar: number, baz: number }


function f4() {
    const rec2 = { foo: "hello", bar: "world", baz: "bye" };
    const lengths2 = mapObject(rec2, s => s.length);  // { foo: number, bar: number, baz: number }
}

