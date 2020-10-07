declare function validate<T>(obj: { [P in keyof T]?: T[P] }): T;

type Foo = {
    a?: number;
    readonly b: string;
}
declare const foo: Foo;
let x = validate(foo);  // { a: number, readonly b: string }
