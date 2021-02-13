// @strict: true
// @declaration: true

type Box<T> = { value: T };

declare function identity<T>(x: T): T;

const arrayMap = <T, U>(f: (x: T) => U) => (a: T[]) => a.map(f);

export const f20: (a: string[]) => number[] = arrayMap(x => x.length);
