function f92<T, K extends keyof T>(x: T, y: T[keyof T], z: T[K]) {
    let a: {} | null | undefined;
    a = x;
    a = y;
    a = z;

    return { a, x, y, z }
}
