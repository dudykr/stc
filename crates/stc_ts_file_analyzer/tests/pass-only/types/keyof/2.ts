// @strictNullChecks: true
// @declaration: true


class A<T> {
	props: T & { foo: string };
}

class B extends A<{ x: number}> {
	f(p: this["props"]) {
		p.x;
	}
}
