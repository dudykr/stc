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

var anb: A & B;
var x: (A & B) | (C & D);

anb = x;
