// @strictNullChecks: true
// @declaration: true

function f5<T, U extends T>(x: T, y: U, k: keyof U) {
    x[k] = y[k];  // Error
    y[k] = x[k];  // Error
}

function f6<T, U extends T, K extends keyof U>(x: T, y: U, k: K) {
    x[k] = y[k];  // Error
    y[k] = x[k];  // Error
}