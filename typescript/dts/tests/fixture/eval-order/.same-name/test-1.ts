export enum A {
    a = 4
}

export enum A {
    b = 4
}

export namespace A {
    export function foo() {
        return A.b
    }
}


export enum A {
    c = 4
}
