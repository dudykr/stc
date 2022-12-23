

declare function foo<T>(x: T, a: T, b: T): T;

foo(1, null as any as unknown, true) // unknown

export { }