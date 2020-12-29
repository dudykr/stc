type Name<T, S> = {
    a: T,
    b: S
}


export type A = Name<string, number>
declare const a: A

export const res1 = a.a;
export const res2 = a.b;