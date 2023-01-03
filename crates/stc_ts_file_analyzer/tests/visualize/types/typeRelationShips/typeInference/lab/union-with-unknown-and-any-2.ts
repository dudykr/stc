

declare function foo<T>(x: T, a: T, b: T): T;

foo(null as any as unknown, null as any as string | number | boolean, null as any) // any

export { }