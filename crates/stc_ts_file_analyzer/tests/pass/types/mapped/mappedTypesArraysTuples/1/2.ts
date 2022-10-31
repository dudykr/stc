//@strict: true

type Box<T> = { value: T };
type Boxified<T> = { [P in keyof T]: Box<T[P]> };

declare function unboxify<T>(x: Boxified<T>): T;

declare let x11: Box<number>[];
export let y11 = unboxify(x11);

y11