// @strictNullChecks: true
// @declaration: true

export function f81<T, K extends keyof T>(t: T, k: K): Partial<T[K]> {
    return t[k];
}
