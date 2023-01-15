// a construct signature that returns any
interface FConstructor {
  new (): any;
}
interface F {
  foo: string;
  bar: number;
}
declare var F: FConstructor;

var obj12: any;
if (obj12 instanceof F) {
  obj12; // any
  obj12.foo;
  obj12.bar;
}
