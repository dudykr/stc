// @strict: true
// @declaration: true

declare function obj(x: object): void;

function f4(x: unknown) {
    if (x == undefined) {
        return;
    }
    if (typeof x === 'object') {
        obj(x);
    }
}

export { }