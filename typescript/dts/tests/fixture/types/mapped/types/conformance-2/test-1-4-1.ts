declare function mapObject<Key extends string, Type, U>(obj: {
    [P in Key]: Type;
}, f: (x: Type) => U): Record<Key, U>;

const rec = { foo: "hello", bar: "world", baz: "bye" };
const lengths = mapObject(rec, s => s.length);  // { foo: number, bar: number, baz: number }
