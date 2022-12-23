

declare function foo<T>(x: T, a: T, b: T): T;

interface I {

}

foo(1, null as any as I, true) // I

export { }