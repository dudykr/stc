class A {
    foo(): string { return ''; }
}

class B extends A {
    bar(): string {
        return '';
    }
}

interface I<U extends T, T extends A> {
    foo: U;
}
var i: I<B, A>;
export var r2 = i.foo.notHere();
