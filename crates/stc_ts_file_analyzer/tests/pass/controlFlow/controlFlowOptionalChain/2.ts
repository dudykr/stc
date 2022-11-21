//@strict: true
//@allowUnreachableCode: false

type Thing = { foo: string | number, bar(): number, baz: object };

function f41(o: Thing | undefined) {
    switch (typeof o?.foo) {
        case "string":
            o.foo;
            break;
        case "number":
            o.foo;
            break;
    }
}

export { }