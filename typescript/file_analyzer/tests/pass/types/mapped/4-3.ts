// Repro from #12900

export interface Base {
    foo: { [key: string]: any };
    bar: any;
    baz: any;
}

export interface E3<T> extends Base {
    foo: Partial<T>; // or other mapped type
}

declare var ee: E3<string>
ee.foo
ee.bar
ee.baz