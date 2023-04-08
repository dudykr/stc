//@strict: true

declare var b: { p: string | null }
if (!b.p) {
    b.p ??= "foo"
}

console.log(b.p)

export { }