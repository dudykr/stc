function f80<T extends { a: { x: any } }>(obj: T) {
    let x2 = obj['a']['x'];  // any

    return x2
}
