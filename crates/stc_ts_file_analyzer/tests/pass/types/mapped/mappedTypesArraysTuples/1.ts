//@strict: true

type Box<T> = { value: T };
type Boxified<T> = { [P in keyof T]: Box<T[P]> };

declare function unboxify<T>(x: Boxified<T>): T;

declare let x10: [Box<number>, Box<string>, ...Box<boolean>[]];
export let y10 = unboxify(x10);


y10