// a construct signature that returns a union type
interface EConstructor {
  new (): E1 | E2;
}
interface E1 {
  foo: string;
  bar1: number;
}
interface E2 {
  foo: string;
  bar2: number;
}
declare var E: EConstructor;

var obj10: any;
if (obj10 instanceof E) {
  obj10; // E1 | E2
  obj10.foo;
}
