// @strictNullChecks: true
// @declaration: true


export function f10<T>(x: T, y: Partial<T>, k: keyof T) {
    y[k] = x[k];
}

