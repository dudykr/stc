//@strict: true

declare var a: string | null
if (!a) {
    a ??= "foo"
}

console.log(a)

export { }
