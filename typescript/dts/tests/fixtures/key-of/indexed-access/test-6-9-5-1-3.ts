function f80<T extends { a: { x: any } }>(obj: T) {
    let a3 = obj['a'] as T['a'];  // T["a"]

    return a3
}
