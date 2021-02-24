// @target: es6
export class B {
    baz(a: string, y = 10) { }
}
export class C extends B {
    foo() { }
    baz(a: string, y: number) {
        super.baz(a, y);
    }
}
export class D extends C {
    constructor() {
        super();
    }

    foo() {
        super.foo();
    }

    baz() {
        super.baz("hello", 10);
    }
}
