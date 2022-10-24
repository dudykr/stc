
interface A {
    a: string
}

interface B {
    b: string
}

declare var obj: A | B


const { a } = obj
a;
export { }