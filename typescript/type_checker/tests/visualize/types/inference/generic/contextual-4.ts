// @strict: true
// @declaration: true

type Box<T> = { value: T };

declare function zip<A, B>(a: A, b: B): [A, B];

declare function flip<X, Y, Z>(f: (x: X, y: Y) => Z): (y: Y, x: X) => Z;

const arrayFilter = <T>(f: (x: T) => boolean) => (a: T[]) => a.filter(f);

export const f30: (a: string[]) => string[] = arrayFilter(x => x.length > 10);
export const f31: <T extends Box<number>>(a: T[]) => T[] = arrayFilter(x => x.value > 10);

export const f40: <A, B>(b: B, a: A) => [A, B] = flip(zip);

// Repro from #16293

type fn = <A>(a: A) => A;
const fn: fn = a => a;
