//@strict: true

function f<T, U>(t: T, u: U) {
    return { ...t, ...u, id: 'id' };
}

let overlap: { id: string, a: number, b: string } =
    f({ a: 1 }, { a: 2, b: 'extra' })

export { }