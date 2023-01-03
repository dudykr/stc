

export declare function foo<T>(x: (t: T) => T): T;


foo((t) => null as any as string | number) // unknown