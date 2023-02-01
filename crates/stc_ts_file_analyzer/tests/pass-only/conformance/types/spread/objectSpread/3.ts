//@strict: true

function f<T, U>(t: T, u: U) {
    return { ...t, ...u, id: 'id' };
}

let overlapConflict: { id: string, a: string } =
    f({ a: 1 }, { a: 'mismatch' })

export { }