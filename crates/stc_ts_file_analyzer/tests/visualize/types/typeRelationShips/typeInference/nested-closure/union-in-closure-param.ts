

export declare function foo<T>(a: (t: T) => (t: T) => T): T;


foo((t) => (t: string | number) => t) // string | number