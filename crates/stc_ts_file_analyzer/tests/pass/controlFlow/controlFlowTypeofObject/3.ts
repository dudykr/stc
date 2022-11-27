// @strict: true
// @declaration: true

declare function obj(x: object): void;

function f3(x: unknown) {
    if (x == null) {
        return;
    }
    if (typeof x === 'object') {
        obj(x);
    }
}


export { }