

export declare function foo<T>(a: (t: T) => (t: T) => T): T;


foo((t) => (t) => null as any as string | number) // unknown