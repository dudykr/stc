

export declare function foo<T>(x: T, a: (t: T) => T, b: T): T;


foo('a', (t) => t, 'b')