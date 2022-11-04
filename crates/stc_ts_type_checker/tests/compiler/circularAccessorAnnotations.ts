// @strict: true
// @declaration: true

declare const c1: {
    get foo(): typeof c1.foo;
}

declare const c2: {
    set foo(value: typeof c2.foo);
}

declare const c3: {
    get foo(): string;
    set foo(value: typeof c3.foo);
}

type T1 = {
    get foo(): T1["foo"];
}

type T2 = {
    set foo(value: T2["foo"]);
}

type T3 = {
    get foo(): string;
    set foo(value: T3["foo"]);
}
