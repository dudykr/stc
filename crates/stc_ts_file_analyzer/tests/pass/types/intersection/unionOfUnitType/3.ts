// @strict

const enum E { A, B, C, D, E, F }

let x0: ('a' | 'b' | 'c') & ('a' | 'b' | 'c');  // 'a' | 'b' | 'c'
let x1: ('a' | 'b' | 'c') & ('b' | 'c' | 'd');  // 'b' | 'c'
let y0: (0 | 1 | 2) & (0 | 1 | 2);  // 0 | 1 | 2
let y1: (0 | 1 | 2) & (1 | 2 | 3);  // 1 | 2
let z0: (E.A | E.B | E.C) & (E.A | E.B | E.C);  // E.A | E.B | E.C
let z1: (E.A | E.B | E.C) & (E.B | E.C | E.D);  // E.B | E.C