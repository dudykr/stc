

declare function foo<T>(x: T, a: T, b: T): T;

enum E {
    A = 's'
}

foo(1, null as any as E, true) // 1

export { }