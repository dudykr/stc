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