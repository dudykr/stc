interface Array<T> {}

var f : { <T>(x:T): T; }

var g : { <S>() : S[]; };
f = g;

var s = f("str").toUpperCase();

console.log(s);
