//@strict: false

type A = { kind: 'a', foo: string };



type D = { kind: 'd', foo: unknown };
type E = { kind: 'e', foo: unknown };

declare function f10<T>(x: { foo: T }): T;

declare let a2: A | D & E;

export let r2 = f10(a2);  // string
