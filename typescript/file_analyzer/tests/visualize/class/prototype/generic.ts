
class C<T, U> {
    foo: T;
    bar: U;
}

class D<T, U> extends C<T, U> {
    baz: T;
    bing: U;
}

var r = C.prototype; // C<any, any>
var ra = r.foo; // any
var r2 = D.prototype; // D<any, any>
var rb = r2.baz; // any

export { }