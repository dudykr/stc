// #23734
export type ObjType = {
    foo: string
    baz: string
    quux: string
}

export const obj: Readonly<ObjType> = {
    foo: 'bar',
    baz: 'qux',
    quux: 'quuz',
}

export const { foo, ...rest } = obj

delete rest.baz
