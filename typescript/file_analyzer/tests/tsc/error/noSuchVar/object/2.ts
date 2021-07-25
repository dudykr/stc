// @target: es6
export var v = {
    bar: 500,
    * foo() {
        yield (bar);
    }
}
