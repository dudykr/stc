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

var a: A;
var b: B;
var c: C;
var d: D;
var anb: A & B = {} as A & B;
var aob: A | B;
var cnd: C & D = {} as C & D;
var cod: C | D;
var x: (A & B) | (C & D);
var y: (A | B) & (C | D) = {} as (A | B) & (C | D);

a = anb; // Ok
b = anb; // Ok

x = anb; // Ok
x = cnd; // Ok

aob = y; // Ok
cod = y; // Ok
