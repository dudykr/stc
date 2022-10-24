// @strict: true

function f3(x: () => void, y: (x?: string) => void) {
    let f = !!true ? x : y;  // (x?: string) => void
    f();
    f('hello');
}
