class C {
    foo: string;
}

class D extends C {
    bar: string;
}

var r = C.prototype;
r.foo;
var r2 = D.prototype;
r2.bar;

export { }
