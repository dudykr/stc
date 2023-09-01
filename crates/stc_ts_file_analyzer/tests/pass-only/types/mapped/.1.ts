// @strictNullChecks: true
// @declaration: true

function f4<T, U extends T, K extends keyof T>(x: T, y: U, k: K) {
    x[k] = y[k];
}
