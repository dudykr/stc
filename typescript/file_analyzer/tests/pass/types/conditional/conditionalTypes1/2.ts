export function f23<T extends string[]>(x: T extends (infer U)[] ? U[] : never) {
    let e = x[0];  // string
}