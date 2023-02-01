//@strict: true
//@allowUnreachableCode: false

type Thing = { foo: string | number, bar(): number, baz: object };

function f40(o: Thing | undefined) {
    switch (o?.foo) {
        case "abc":
            o.foo;
            break;
        case 42:
            o.foo;
            break;
    }
}

export { }