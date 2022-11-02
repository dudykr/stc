//@strict: true
//@allowUnreachableCode: false

function fail(message?: string): never {
    throw new Error(message);
}

function f01(x: string | undefined) {
    if (x === undefined) fail("undefined argument");
    x.length;  // string
}

export { }