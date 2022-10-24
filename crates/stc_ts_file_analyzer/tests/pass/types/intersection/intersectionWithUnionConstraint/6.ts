// @strict: true

export function f2<T extends string | number | undefined, U extends string | null | undefined>(x: T & U) {
    let y6: null | undefined = x;    // Error
}
