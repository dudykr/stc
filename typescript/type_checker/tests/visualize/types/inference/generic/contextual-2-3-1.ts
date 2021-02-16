// @strict: true
// @declaration: true

type Box<T> = { value: T };

declare function id<T>(a: T): T;

declare function unbox<W>(x: Box<W>): W;

export const f: <T>(x: Box<T>) => T = id(a => unbox(a));
