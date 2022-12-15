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

var cod: C | D;
var y: (A | B) & (C | D);

y = cod;
