// @strict: true
// @declaration: true

type Box<T> = { value: T };

const arrayFilter = <T>(f: (x: T) => boolean) => (a: T[]) => a.filter(f);

export const f31: <T extends Box<number>>(a: T[]) => T[] = arrayFilter(x => x.value > 10);

