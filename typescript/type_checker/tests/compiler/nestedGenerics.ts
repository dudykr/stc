interface Foo<T> {
	t: T;
}

var f: Foo<Foo<number>>;