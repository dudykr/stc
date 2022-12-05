const enum Tag1 {
  a = 1,
  b = 1,
}

declare let s1: Tag1.a & Tag1.b;

s1 = 1;
