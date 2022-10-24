// @strict: true

function f5(x: (x: string | undefined) => void, y: (x?: 'hello') => void) {
    let f = !!true ? x : y;  // (x?: 'hello') => void
    f();
    f('hello');
}
