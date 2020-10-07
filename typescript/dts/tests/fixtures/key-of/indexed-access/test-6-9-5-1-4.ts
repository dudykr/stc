function f80<T extends { a: { x: any } }>(obj: T) {
    let x1 = obj.a.x;  // any

    return x1
}
