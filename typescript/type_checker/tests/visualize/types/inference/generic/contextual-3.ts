// @strict: true
// @declaration: true

type Box<T> = { value: T };

declare function identity<T>(x: T): T;

const arrayMap = <T, U>(f: (x: T) => U) => (a: T[]) => a.map(f);

export const f20: (a: string[]) => number[] = arrayMap(x => x.length);
export const f21: <A>(a: A[]) => A[][] = arrayMap(x => [x]);
export const f22: <A>(a: A[]) => A[] = arrayMap(identity);
export const f23: <A>(a: A[]) => Box<A>[] = arrayMap(value => ({ value }));
