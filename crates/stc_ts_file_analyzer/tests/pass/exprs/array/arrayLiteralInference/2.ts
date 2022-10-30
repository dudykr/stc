//@strict: true
//@target: es2015

declare function foo<T>(...args: T[]): T[];
export let b1: { x: boolean }[] = foo({ x: true }, { x: false });
