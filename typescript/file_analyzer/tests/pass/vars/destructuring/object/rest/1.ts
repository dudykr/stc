type ObjType = {
    foo: string
    baz: string
    quux: string
}

const obj: Readonly<ObjType> = {
    foo: 'bar',
    baz: 'qux',
    quux: 'quuz',
}

export const { foo, ...rest } = obj
