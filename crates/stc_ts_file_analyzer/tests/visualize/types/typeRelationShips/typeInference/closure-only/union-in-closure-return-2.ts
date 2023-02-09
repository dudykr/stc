

export declare function foo<T>(x: (t: T) => T): T;

declare function bar(t: any): string | number;

foo(bar) // string | number