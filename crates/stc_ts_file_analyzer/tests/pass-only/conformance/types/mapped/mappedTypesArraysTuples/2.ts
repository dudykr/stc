// @strict: true
// @declaration: true

type Box<T> = { value: T };


declare function nonpartial<T>(x: Partial<T>): T;

declare let x20: [number | undefined, string?, ...boolean[]];
export let y20 = nonpartial(x20);

