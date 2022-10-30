//@strict: true

type Box<T> = { value: T };
type Boxified<T> = { [P in keyof T]: Box<T[P]> };

declare function unboxify<T>(x: Boxified<T>): T;

declare let x12: { a: Box<number>, b: Box<string[]> };
let y12 = unboxify(x12);

declare function nonpartial<T>(x: Partial<T>): T;

declare let x20: [number | undefined, string?, ...boolean[]];
export let y20 = nonpartial(x20);

