function f80<T extends { a: { x: any } }>(obj: T) {
    let x3 = obj['a']['x'] as T['a']['x'];  // T["a"]["x"]

    return x3
}
