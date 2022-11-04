//@strict: false

type A = { kind: 'a', foo: string };
type B = { kind: 'b', foo: number };
type C = { kind: 'c', foo: number };

declare let x: A | (B & C);  // A
export let a: A = x;
