// @strictNullChecks: true
// @declaration: true

function f6<T, U extends T, K extends keyof U>(x: T, y: U, k: K) {
    x[k] = y[k];  // Error
    y[k] = x[k];  // Error
}
