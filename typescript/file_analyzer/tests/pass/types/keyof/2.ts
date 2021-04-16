// @strictNullChecks: true
// @declaration: true

class A {
    a: string
    c: number
}

class B {
    b: string
    c: number
}

export type Keys = keyof (A | B)

declare let c: 'c'
declare var keys: Keys

c = keys
keys = c
