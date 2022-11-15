// @strict: true

const enum Tag1 {
  a = "a",
  b = "b",
}

declare let s1: string & Tag1;
declare let s2: string;

s1 = Tag1.a;
s2 = s1;
