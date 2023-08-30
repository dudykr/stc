// @strictNullChecks: true
// @declaration: true

function f1<T>(x: T, k: keyof T) {
    return x[k];
}

function f2<T, K extends keyof T>(x: T, k: K) {
    return x[k];
}

function f3<T, U extends T>(x: T, y: U, k: keyof T) {
    // x[k] = y[k];
}

function f10<T>(x: T, y: Partial<T>, k: keyof T) {
    y[k] = x[k];
}

function f12<T, U extends T>(x: T, y: Partial<U>, k: keyof T) {
    x[k] = y[k];  // Error
    y[k] = x[k];  // Error
}   