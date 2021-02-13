// @strict: true
// @declaration: true

type Box<T> = { value: T };

declare function compose<A, B, C>(f: (a: A) => B, g: (b: B) => C): (a: A) => C;

declare function list<T>(a: T): T[];

declare function unlist<T>(a: T[]): T;

declare function box<V>(x: V): Box<V>;

declare function unbox<W>(x: Box<W>): W;

export const f10: <T>(x: T) => Box<T[]> = compose(a => list(a), b => box(b));
export const f11: <T>(x: T) => Box<T[]> = compose(list, box);
export const f12: <T>(x: Box<T[]>) => T = compose(a => unbox(a), b => unlist(b));
export const f13: <T>(x: Box<T[]>) => T = compose(unbox, unlist);
