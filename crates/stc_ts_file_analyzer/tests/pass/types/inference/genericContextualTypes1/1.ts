//@strict: true

export const arrayMap = <T, U>(f: (x: T) => U) => (a: T[]) => a.map(f);
