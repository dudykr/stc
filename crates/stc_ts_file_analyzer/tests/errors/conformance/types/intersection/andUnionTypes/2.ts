interface A {
  a: string;
}
interface B {
  b: string;
}

var a: A;
var b: B;
var anb: A & B;

anb = b;
