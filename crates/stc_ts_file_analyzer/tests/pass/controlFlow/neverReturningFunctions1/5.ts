//@strict: true
//@allowUnreachableCode: false

export function f43() {
    const fail = (): never => { throw new Error(); };
    const f = [fail];
    fail();  // No effect (missing type annotation)
    f[0]();  // No effect (not a dotted name)
    f;
}

