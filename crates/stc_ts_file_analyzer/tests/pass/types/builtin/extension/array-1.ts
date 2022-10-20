interface Array<T> {
    equalsShallow(): any
}

declare const a: (string | number)[]

let x = a.equalsShallow();
