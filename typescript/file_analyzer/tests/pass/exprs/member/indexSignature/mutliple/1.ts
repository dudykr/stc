function foo<T>(x: T) {
    return x;
}

export function other3<T extends U, U extends Date>(arg: T) {
    var b: {
        [x: string]: Object;
        [x: number]: T;
    };
    var r2 = foo(b);
    var d = r2[1];
    var e = r2['1'];
    var u: U = r2[1]; // ok
}

