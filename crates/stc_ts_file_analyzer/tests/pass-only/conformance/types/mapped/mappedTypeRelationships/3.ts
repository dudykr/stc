// @strictNullChecks: true
// @declaration: true


export function f10<T>(x: T, y: Partial<T>, k: keyof T) {
    x[k] = y[k];  // Error
    y[k] = x[k];
}

