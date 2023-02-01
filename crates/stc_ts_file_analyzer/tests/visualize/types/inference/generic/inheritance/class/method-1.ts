class C<T extends { length: number }> {
    constructor(x: T) { }
    foo<U extends T>(x: U) {
        function bar<V extends U>(x: V) {
            return x;
        }
        return bar;
    }
}

var c = new C({ length: 2 });
export var r = c.foo({ length: 3, charAt: (x: number) => { '' } });
export var r2 = r('');