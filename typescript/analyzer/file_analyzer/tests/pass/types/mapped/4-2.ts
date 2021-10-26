// Repro from #12900

export interface Base {
    foo: { [key: string]: any };
    bar: any;
    baz: any;
}

export interface Something { name: string, value: string };
export interface E2 extends Base {
    foo: Partial<Something>;  // or other mapped type
}


declare var e2: E2
e2.foo
e2.foo.name
e2.foo.value
e2.bar
e2.baz