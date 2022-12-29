//@strict: true


export declare function foo2<T>(x: T, a: (t: T) => T, b: (t: T) => T): T;

foo2(null, (t: Object) => t, (t: string) => t) // string