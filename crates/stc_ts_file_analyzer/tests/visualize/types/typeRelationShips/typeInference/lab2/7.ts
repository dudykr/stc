

declare function f3<T>(a: T, b: () => T): T

f3('a', () => null as any as string | number) // string | number

export { }