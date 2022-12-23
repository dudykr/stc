

export declare function foo<T>(a: (t: T) => (t: T) => T): T;


foo((t) => (a) => a as any as Object) // "unknown"