interface Foo {
    a: {
        b: {
            c: {
                d: string
            }
        }
    }
}

let q: Foo["a"] | undefined;
const x: Foo = (void 0, {
    a: q = {
        b: ({
            c: {
                d: 42
            }
        })
    }
});
