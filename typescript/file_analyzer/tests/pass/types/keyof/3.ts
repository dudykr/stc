// @strictNullChecks: true
// @declaration: true

class A {
    a: string
    c: number
}

export type Keys = keyof (A | any)

declare var keys: Keys

declare let foo: string | number | symbol

foo = keys
keys = foo