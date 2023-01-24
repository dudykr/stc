//@strict: true

function f<T, U>(t: T, u: U) {
    return { ...t, ...u, id: 'id' };
}

let exclusive: { id: string, a: number, b: string, c: string, d: boolean } =
    f({ a: 1, b: 'yes' }, { c: 'no', d: false })

export { }