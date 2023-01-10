

export declare function foo<T>(x: (t: T) => T): T;



foo((t: string | number) => t) // string | number