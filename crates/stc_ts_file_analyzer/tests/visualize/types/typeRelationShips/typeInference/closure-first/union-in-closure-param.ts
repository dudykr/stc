

export declare function foo<T>(x: (t: T) => T, a: T, b: T): T;



foo((t: string | number) => t, 'a', 'a') // string | number