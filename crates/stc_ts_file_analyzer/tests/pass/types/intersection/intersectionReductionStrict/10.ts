const enum Tag1 {
  a = "a",
  b = 1,
}

declare let s1: string & Tag1.a;
declare let s2: number & Tag1.b;

s1 = Tag1.a;
s2 = Tag1.b;
