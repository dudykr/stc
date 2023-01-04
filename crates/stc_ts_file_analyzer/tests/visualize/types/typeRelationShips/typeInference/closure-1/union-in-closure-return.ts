

export declare function foo<T>(x: T, a: (t: T) => T, b: T): T;


foo('a', (t) => null as any as string | number, 'a') // "a"