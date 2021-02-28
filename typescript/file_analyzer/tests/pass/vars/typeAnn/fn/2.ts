type Box<T> = { value: T };

declare function unlist<T>(a: T[]): T;

declare function unbox<W>(x: Box<W>): W;

export const f12: <T>(x: Box<T[]>) => T = compose(a => unbox(a), b => unlist(b));
