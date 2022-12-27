function foo2<T, U>(x: T, a: (x: T) => U, b: (x: T) => U) {
    var r: (x: T) => U;
    return r;
}

var r8 = foo2('', (x) => '', (x) => null); // string => string | null
var r9 = foo2(null, (x) => '', (x) => ''); // null => string
var r10 = foo2(null, (x: Object) => '', (x: string) => ''); // string => string