interface Foo {
    getFoo(n: number): void;
    getFoo(s: string): void;
}

var foo: Foo;
foo.getFoo = bar => { };