// @strict: true

type A = { a: string };
type B = { b: string };

declare let sa1: { x: A & B };
declare let sa2: { x: A } & { x: B };
declare let ta2: { [key: string]: A } & { [key: string]: B };

ta2 = sa1;
ta2 = sa2;


export { }
