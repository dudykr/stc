

export declare function foo<T>(x: (t: T) => T, a: T, b: T): T;


foo((t: Object) => t, 'a', 'a') // Object