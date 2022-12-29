//@strict: true

export declare function foo2<T>(x: T, a: (t: T) => T, b: (t: T) => T): T;

foo2('a', (t: Object) => t, (t: string) => t) // Object