// Repro from #12900

export interface Base {
    foo: { [key: string]: any };
    bar: any;
    baz: any;
}

export interface E1<T> extends Base {
    foo: T;
}

export interface Something { name: string, value: string };
export interface E2 extends Base {
    foo: Partial<Something>;  // or other mapped type
}

export interface E3<T> extends Base {
    foo: Partial<T>; // or other mapped type
}
