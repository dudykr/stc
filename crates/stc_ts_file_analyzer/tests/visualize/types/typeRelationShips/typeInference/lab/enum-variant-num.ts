

declare function foo<T>(x: T, a: T, b: T): T;

enum E {
    A
}

foo(1, null as any as E.A, true) // 1

export { }