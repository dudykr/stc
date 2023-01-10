

declare function f2<T>(a: T, b: (a: T) => T): T

f2('a', (t) => null as any as string | number) // "a"

export { }