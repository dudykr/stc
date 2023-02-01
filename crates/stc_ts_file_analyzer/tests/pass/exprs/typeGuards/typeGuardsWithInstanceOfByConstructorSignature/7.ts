// a type with a prototype, it overrides the construct signature
interface GConstructor {
  prototype: G1; // high priority
  new (): G2; // low priority
}
interface G1 {
  foo1: number;
}
interface G2 {
  foo2: boolean;
}
declare var G: GConstructor;

var obj14: any;
if (obj14 instanceof G) {
  obj14; // G1
  obj14.foo1;
}
