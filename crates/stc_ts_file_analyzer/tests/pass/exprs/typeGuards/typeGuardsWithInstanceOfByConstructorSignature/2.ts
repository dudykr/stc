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

var obj13: G1 | G2;
if (obj13 instanceof G) {
  // narrowed to G1. G1 is return type of prototype property.
  obj13.foo1;
}
