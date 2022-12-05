const enum Tag1 {
  a = 1,
  b = 2,
}

declare let s1: Tag1.a & Tag1.b;
declare let s2: never;

s1 = s2;
s2 = s1;
