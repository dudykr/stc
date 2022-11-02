//@strict: true
//@allowUnreachableCode: false

function f11(x: string | undefined, fail: (message?: string) => never) {
    if (x === undefined) fail("undefined argument");
    x.length;  // string
}

export { }
