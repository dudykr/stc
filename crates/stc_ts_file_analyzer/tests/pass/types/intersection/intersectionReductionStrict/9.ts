// @strict: true

const enum Tag1 {
  a = "a",
  b = 1,
}

declare let s1: number & Tag1;
declare let s2: number;

s1 = Tag1.b;
s2 = s1;
