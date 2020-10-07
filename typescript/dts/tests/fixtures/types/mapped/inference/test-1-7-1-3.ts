declare function validateAndClone<T>(obj: { readonly [P in keyof T]?: T[P] }): T;

type Foo = {
    a?: number;
    readonly b: string;
}
declare const foo: Foo;
let z = validateAndClone(foo);  // { a: number, b: string }
