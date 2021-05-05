function foo<T>(a: (x: T) => T, b: (x: T) => T) {
    var r: (x: T) => T;
    return r;
}

function other<T>(x: T) {
    var r6 = foo((a: T) => a, (b: T) => b); // T => T
    var r6b = foo((a) => a, (b) => b); // {} => {}
}
