// @strict: true

const enum Tag1 {
  a,
  b,
}

declare let s1: number & Tag1;
declare let s2: number;

s1 = Tag1.a;
s2 = s1;
