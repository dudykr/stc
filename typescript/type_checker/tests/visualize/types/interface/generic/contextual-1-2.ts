declare function wrap<A, B>(f: (a: A) => B): (a: A) => B;

declare function list<T>(a: T): T[];

export const f03: <A>(x: A) => A[] = wrap(x => [x]);
