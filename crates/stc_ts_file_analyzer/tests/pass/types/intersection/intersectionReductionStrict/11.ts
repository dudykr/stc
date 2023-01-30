const enum Tag1 {
  a = 1,
  b = 1,
}

declare let s1: Tag1.a & Tag1.b;
declare let s2: Tag1.a & 2;
s1 = 1;
s2 = 1 as never;
