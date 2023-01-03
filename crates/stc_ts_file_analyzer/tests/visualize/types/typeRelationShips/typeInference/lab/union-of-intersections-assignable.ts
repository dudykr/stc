

declare function foo<T>(x: T, a: T, b: T): T;

foo({ a: '', b: 1 }, null as any as ({ a: string } & { b: number }) | ({ c: bigint } & { d: boolean }), true) // union of intersections

export { }