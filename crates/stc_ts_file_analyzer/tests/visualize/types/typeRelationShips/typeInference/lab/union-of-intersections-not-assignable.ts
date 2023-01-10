

declare function foo<T>(x: T, a: T, b: T): T;

foo(1, null as any as ({ a: string } & { b: number }) | ({ c: bigint } & { d: boolean }), true) // 1`

export { }