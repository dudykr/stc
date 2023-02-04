//@strict: true

export type Foo = Awaited<Promise<{ x: number }>>

declare var foo: Foo
var foo = { x: 1 }