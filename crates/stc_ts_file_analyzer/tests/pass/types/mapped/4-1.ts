// Repro from #12900

export interface Base {
    foo: { [key: string]: any };
    bar: any;
    baz: any;
}

export interface E1<T> extends Base {
    foo: T;
}

declare var e1: E1<string>
e1.foo
e1.bar
e1.baz