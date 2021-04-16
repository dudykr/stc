// @strictNullChecks: true
// @declaration: true

export type Keys = keyof any

declare var keys: Keys

declare let foo: string | number | symbol

foo = keys
keys = foo