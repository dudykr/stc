class C {
    constructor(x: number) { }
    static foo: number;
    static bar() {
        // type of this is the constructor function type
        var t = this;
        return this;
    }
}

var t = C.bar();
// all ok
var r2 = t.foo + 1;
var r3 = t.bar();
var r4 = new t(1);

export { }