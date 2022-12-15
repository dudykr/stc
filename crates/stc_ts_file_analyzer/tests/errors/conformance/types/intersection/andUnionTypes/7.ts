interface A {
  a: string;
}
interface B {
  b: string;
}
interface C {
  c: string;
}
interface D {
  d: string;
}

var cnd: C & D;
var x: (A & B) | (C & D);

cnd = x;
