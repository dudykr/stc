

function foo(cb: (a: string) => string): 2;
function foo(cb: (a: number) => string): 1;
function foo<T, R>(cb: (a: T) => R): 3;
function foo(...args: Function[]) {
    return args
}

let x = foo(a => `${a}`)