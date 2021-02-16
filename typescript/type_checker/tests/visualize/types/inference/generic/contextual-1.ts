// @strict: true
// @declaration: true

type Box<T> = { value: T };

declare function wrap<A, B>(f: (a: A) => B): (a: A) => B;

declare function list<T>(a: T): T[];

export const f00: <A>(x: A) => A[] = list;
export const f01: <A>(x: A) => A[] = x => [x];
export const f02: <A>(x: A) => A[] = wrap(list);
export const f03: <A>(x: A) => A[] = wrap(x => [x]);

