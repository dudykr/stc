
export interface A {
    a: number
}

export interface B extends A {
    b: string
}

declare const i: B

export const a = i.a;
export const b = i.b;


