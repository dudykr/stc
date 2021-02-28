function f14<T>(x: { a: 0; b: string } | { a: T, c: number }) {
    if (x.a === 0) {
        x
    }
}

export { }