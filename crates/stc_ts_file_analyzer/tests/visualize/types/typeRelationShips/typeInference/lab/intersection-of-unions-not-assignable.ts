

declare function foo<T>(x: T, a: T, b: T): T;

foo(1, null as any as (string | number) & (string | boolean), true) // 1

export { }