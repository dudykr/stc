function f80<T extends { a: { x: any } }>(obj: T) {
    let a2 = obj['a'];  // { x: any }

    return a2
}
