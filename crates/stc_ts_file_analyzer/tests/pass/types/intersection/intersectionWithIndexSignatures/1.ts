// @strict: true

type A = { a: string };
type B = { b: string };

declare let sa1: { x: A & B };
declare let sa2: { x: A } & { x: B };
declare let ta1: { [key: string]: A & B };

ta1 = sa1;
ta1 = sa2;
export { }
