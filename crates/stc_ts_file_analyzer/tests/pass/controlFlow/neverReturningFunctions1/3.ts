//@strict: true
//@allowUnreachableCode: false

namespace Debug {
    export declare function fail(message?: string): never;
}

function f21(x: string | undefined) {
    if (x === undefined) Debug.fail("undefined argument");
    x.length;  // string
}

export { }
