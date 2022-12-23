

declare function foo<T>(x: T, a: T, b: T): T;

interface I {
    prop: string
}


foo(1, null as any as I, true) // 1

export { }