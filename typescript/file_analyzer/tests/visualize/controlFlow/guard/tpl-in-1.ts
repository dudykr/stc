declare const obj: { test: string } | {}
if (`test` in obj) {
    obj.test.slice(0)
}


export { }