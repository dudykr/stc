function f80<T extends { a: { x: any } }>(obj: T) {
    let a1 = obj.a;  // { x: any }

    return a1;
}
