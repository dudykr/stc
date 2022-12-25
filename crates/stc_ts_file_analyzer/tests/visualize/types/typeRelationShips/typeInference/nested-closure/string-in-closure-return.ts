

export declare function foo<T>(a: (t: T) => (t: T) => T): T;


foo((t) => (t) => 'foo') // unknown