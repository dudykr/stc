// @strict: true

const enum Tag1 {
  a = "a",
  b = "b",
}
const enum Tag2 {}

declare let s1: string & Tag1;
declare let s2: string & Tag2;

s1 = s2;
s2 = s1;
