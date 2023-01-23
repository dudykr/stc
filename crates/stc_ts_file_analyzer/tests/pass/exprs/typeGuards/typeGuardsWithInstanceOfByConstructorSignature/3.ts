// has multiple construct signature
interface CConstructor {
  new (value: string): C1;
  new (value: number): C2;
}
interface C1 {
  foo: string;
  c: string;
  bar1: number;
}
interface C2 {
  foo: string;
  c: string;
  bar2: number;
}
declare var C: CConstructor;

var obj6: any;
if (obj6 instanceof C) {
  obj6; // C1 | C2
  obj6.foo;
}
