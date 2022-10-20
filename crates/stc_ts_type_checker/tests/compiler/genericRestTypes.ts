// @strict: true

// Repro from #25793

// Gets the parameters of a function type as a tuple
// Removes the first element from a tuple
type Tail<T extends any[]> = ((...args: T) => any) extends ((head: any, ...tail: infer U) => any) ? U : never;

type MyFunctionType = (foo: number, bar: string) => boolean;

type Explicit = (...args: Tail<Parameters<MyFunctionType>>) => ReturnType<MyFunctionType>; // (bar: string) => boolean

type Bind1<T extends (head: any, ...tail: any[]) => any> = (...args: Tail<Parameters<T>>) => ReturnType<T>;
type Generic = Bind1<MyFunctionType>; // (bar: string) => boolean

function assignmentWithComplexRest<T extends any[]>() {
    const fn1: (x: string, ...rest: T) => void = (x, ..._) => x;
    const fn2: (...args: never) => void = fn1;
}

function assignmentWithComplexRest2<T extends any[]>() {
    const fn1: (cb: (x: string, ...rest: T) => void) => void = (cb) => {};
    const fn2: (cb: (...args: never) => void) => void = fn1;
}

function assignmentWithComplexRest3<T extends any[]>() {
    const fn1: (x: string, ...rest: T) => void = (x, ..._) => x;
    const fn2: (...args: {x: "a"} & {x: "b"}) => void = fn1;
}