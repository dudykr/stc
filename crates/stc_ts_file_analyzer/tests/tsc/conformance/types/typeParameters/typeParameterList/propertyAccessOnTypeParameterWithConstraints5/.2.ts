class A {
    foo(): string { return ''; }
}

class B extends A {
    bar(): string {
        return '';
    }
}

class C<U extends T, T extends A> {
    f() {
        var x: U;
        var a = x['foo'](); // should be string
        return a + x.foo() + x.notHere();
    }
}


interface I<U extends T, T extends A> {
    foo: U;
}

var a: {
    <U extends T, T extends A>(): U;
}
// BUG 794164
export var r3: string = a().notHere();
