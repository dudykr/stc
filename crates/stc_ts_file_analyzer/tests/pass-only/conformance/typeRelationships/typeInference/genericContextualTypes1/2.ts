// @strict: true
// @declaration: true

type Box<T> = { value: T };

declare function compose<A, B, C>(f: (a: A) => B, g: (b: B) => C): (a: A) => C;

declare function unlist<T>(a: T[]): T;

declare function unbox<W>(x: Box<W>): W;

export const f12: <T>(x: Box<T[]>) => T = compose(a => unbox(a), b => unlist(b));
