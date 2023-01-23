interface EConstructor {
  new (): E1 | E2;
}

interface A {
  foo: string;
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

var obj9: E1 | A;
if (obj9 instanceof E) {
  obj9.foo;
  obj9.bar1;
}
