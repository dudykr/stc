function foo2<T, U>(x: T, a: (x: T) => U, b: (x: T) => U) {
    var r: (x: T) => U;
    return r;
}

var r11 = foo2(x, (a1: (y: string) => string) => (n: Object) => 1, (a2: (z: string) => string) => 2); // error
var r12 = foo2(x, (a1: (y: string) => boolean) => (n: Object) => 1, (a2: (z: string) => boolean) => 2); // error