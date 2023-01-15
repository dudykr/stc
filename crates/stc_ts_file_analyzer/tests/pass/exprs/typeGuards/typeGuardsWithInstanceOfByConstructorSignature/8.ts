// a type with a prototype that has any type
interface HConstructor {
  prototype: any; // high priority, but any type is ignored. interface has implicit `prototype: any`.
  new (): H; // low priority
}
interface H {
  foo: number;
}
declare var H: HConstructor;

var obj16: any;
if (obj16 instanceof H) {
  obj16; // H
  obj16.foo;
}
