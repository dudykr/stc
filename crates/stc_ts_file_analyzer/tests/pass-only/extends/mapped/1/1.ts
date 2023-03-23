//@strict: true

export type Foo = null extends ((value: {
    x: number;
}, ...args: any) => any) ? true : false;

declare var foo: Foo
var foo = false as const