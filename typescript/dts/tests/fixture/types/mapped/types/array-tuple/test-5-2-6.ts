// @strict: true
// @declaration: true

type Box<T> = { value: T };
type Boxified<T> = { [P in keyof T]: Box<T[P]> };

type T40 = Boxified<A | A[] | ReadonlyArray<A> | [A, B] | string | string[]>;

type ReadWrite<T> = { -readonly [P in keyof T]: T[P] };

type A = { a: string };
type B = { b: string };

declare function nonpartial<T>(x: Partial<T>): T;

declare let x22: { a: number | undefined, b?: string[] };
let y22 = nonpartial(x22);
