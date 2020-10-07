declare function mapObject<K extends string, T, U>(obj: Record<K, T>, f: (x: T) => U): Record<K, U>;

const rec = { foo: "hello", bar: "world", baz: "bye" };
const lengths = mapObject(rec, s => s.length);  // { foo: number, bar: number, baz: number }
