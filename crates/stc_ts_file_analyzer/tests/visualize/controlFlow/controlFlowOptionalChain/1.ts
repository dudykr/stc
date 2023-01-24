//@strict: true
//@allowUnreachableCode: false


type Thing = { foo: string | number, bar(): number, baz: object };

export function f40(o: Thing | undefined) {
    switch (o?.foo) {
        case "abc":
            break;
        case 42:
            break;
        case undefined:
            o.foo;  // Error
            break;
        default:
            o.foo;  // Error
            break;
    }
}
