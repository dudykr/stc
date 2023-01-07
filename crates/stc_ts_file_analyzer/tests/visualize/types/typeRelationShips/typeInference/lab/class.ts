

declare function foo<T>(x: T, a: T, b: T): T;

class C { }

foo(1, null as any as C, true) // C

export { }