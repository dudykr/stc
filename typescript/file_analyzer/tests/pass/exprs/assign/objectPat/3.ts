function f3<T, K1 extends keyof T, K2 extends keyof T>(obj: T, k1: K1, k2: K2) {
    let { [k1]: a1, [k2]: a2, ...r1 } = obj;
}

export { }