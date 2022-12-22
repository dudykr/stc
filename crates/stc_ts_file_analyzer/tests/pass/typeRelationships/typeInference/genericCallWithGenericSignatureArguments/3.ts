function foo2<T, U>(x: T, a: (x: T) => U, b: (x: T) => U) {
    var r: (x: T) => U;
    return r;
}

var r8 = foo2('', (x) => '', (x) => null); // string => string
var r9 = foo2(null, (x) => '', (x) => ''); // any => any
var r10 = foo2(null, (x: Object) => '', (x: string) => ''); // Object => Object