// @strictNullChecks: true
// @declaration: true

export function f82<T, K1 extends keyof T, K2 extends keyof T[K1]>(t: T, k1: K1, k2: K2): Partial<T[K1][K2]> {
    return t[k1][k2];
}
