// @strict: true
// @declaration: true

type T0 = string & `${string}`;  // string
type T1 = string & `${number}`;  // `${number}
type T2 = string & `${bigint}`;  // `${bigint}
type T3<T extends string> = string & `${T}`;  // `${T}
type T4<T extends string> = string & `${Capitalize<`${T}`>}`;  // `${Capitalize<T>}`

function f1(a: boolean[], x: `${number}`) {
    let s = a[x];  // boolean
}

function f2(a: boolean[], x: number | `${number}`) {
    let s = a[x];  // boolean
}

type T10 = boolean[][`${number}`];  // boolean
type T11 = boolean[][number | `${number}`];  // boolean

type T20<T extends number | `${number}`> = T;
type T21<T extends unknown[]> = { [K in keyof T]: T20<K> };

type Container<T> = {
    value: T
}
