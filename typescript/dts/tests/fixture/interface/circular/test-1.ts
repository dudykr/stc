
export interface A {
    b: B
}

export interface B {
    a: A
}


declare const b: B
export const aOfB = b.a