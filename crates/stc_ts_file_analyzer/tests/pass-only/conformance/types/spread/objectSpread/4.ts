//@strict: true

function f<T, U>(t: T, u: U) {
    return { ...t, ...u, id: 'id' };
}

let overwriteId: { id: string, a: number, c: number, d: string } =
    f({ a: 1, id: true }, { c: 1, d: 'no' })

export { }