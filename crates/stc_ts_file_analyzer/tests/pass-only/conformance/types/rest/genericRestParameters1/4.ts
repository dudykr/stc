// @strict: true
// @declaration: true

declare function f10<T extends unknown[]>(...args: T): T;

export function g10<U extends string[], V extends [number, number]>(u: U, v: V) {
    let x4 = f10(...u, ...v);  // (string | number)[]
}

