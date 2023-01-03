

declare function f4<T>(a: T, b: (s: string) => T): T

f4('a', (t) => null as any as string | number) // string | number

export { }