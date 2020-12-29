declare function validate<T>(obj: { [P in keyof T]?: T[P] }): T;

declare function clone<T>(obj: { readonly [P in keyof T]: T[P] }): T;

declare function validateAndClone<T>(obj: { readonly [P in keyof T]?: T[P] }): T;

type Foo = {
    a?: number;
    readonly b: string;
}
declare const foo: Foo;
let x = validate(foo);  // { a: number, readonly b: string }
let y = clone(foo);  // { a?: number, b: string }
let z = validateAndClone(foo);  // { a: number, b: string }
