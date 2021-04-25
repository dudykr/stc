export function f22<T>(x: T extends (infer U)[] ? U[] : never) {
    let e = x[0];  // {}
}

