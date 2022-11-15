interface AConstructor {
    new(): A;
}
interface A {
    foo: string;
}
declare var A: AConstructor;

declare var obj1: A | string;
if (obj1 instanceof A) { // narrowed to A.
    obj1.foo;
}

export { }