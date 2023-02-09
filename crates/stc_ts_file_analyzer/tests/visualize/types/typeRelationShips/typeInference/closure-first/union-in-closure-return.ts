

export declare function foo<T>(x: (t: T) => T, a: T, b: T): T;


foo((t) => null as any as string | number, 'a', 'b') // "a" | "b"