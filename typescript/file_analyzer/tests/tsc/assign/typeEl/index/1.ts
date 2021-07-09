
interface A {
    [s: string]: number
}
interface B {
    [b: string]: number
    [a: number]: number
}


declare var a: A
declare var b: B


a = b
b = a