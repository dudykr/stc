//@strict: false

type A = { kind: 'a', foo: string };
type D = { kind: 'd', foo: unknown };

declare function f10<T>(x: { foo: T }): T;

declare let a1: A | D;

export let r1 = f10(a1);  // unknown
