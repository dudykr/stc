// @strict: true

function f4(x: (x: string | undefined) => void, y: (x?: string) => void) {
    let f = !!true ? x : y;  // (x?: string) => void
    f();
    f('hello');
}

