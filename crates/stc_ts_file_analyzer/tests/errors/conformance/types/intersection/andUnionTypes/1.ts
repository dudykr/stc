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
var anb: A & B;
var aob: A | B;
var cnd: C & D;
var cod: C | D;
var x: (A & B) | (C & D);
var y: (A | B) & (C | D);

anb = a;
anb = b;
x = aob;
x = cod;
anb = x;
aob = x;
cnd = x;
cod = x;
y = anb;
y = aob;
y = cnd;
y = cod;
anb = y;
cnd = y;
