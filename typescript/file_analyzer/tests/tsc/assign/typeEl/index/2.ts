
interface A {
    [s: string]: number
}
interface B {
    [a: number]: string
}


declare var a: A
declare var b: B


a = b
b = a

export { }
