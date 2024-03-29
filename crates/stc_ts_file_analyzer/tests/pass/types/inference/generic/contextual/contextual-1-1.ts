declare function wrap<A, B>(f: (a: A) => B): (a: A) => B;

declare function list<T>(a: T): T[];

export const f02: <A>(x: A) => A[] = wrap(list);
