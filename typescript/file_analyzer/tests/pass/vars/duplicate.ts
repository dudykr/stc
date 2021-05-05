function foo<T>(a: (x: T) => T, b: (x: T) => T) {
    var r: (x: T) => T;
    return r;
}

function other<T>(x: T) {
    var r6 = foo((a: T) => a, (b: T) => b); // T => T
    var r6b = foo((a) => a, (b) => b); // {} => {}
}

function other2<T extends Date>(x: T) {
    var r7 = foo((a: T) => a, (b: T) => b); // T => T
    var r7b = foo((a) => a, (b) => b); // {} => {}
    var r8 = r7(null);
    // BUG 835518
    //var r9 = r7(new Date());
}