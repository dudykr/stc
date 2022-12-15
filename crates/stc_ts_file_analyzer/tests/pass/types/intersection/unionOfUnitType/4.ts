// @strict

const enum E { A, B, C, D, E, F }

let x5: ('a' | 'b' | 'c') & ('b' | 'c' | 'd') & ('c' | 'd' | 'e') & ('d' | 'e' | 'f');  // never

let y5: (0 | 1 | 2) & (1 | 2 | 3) & (2 | 3 | 4) & (3 | 4 | 5);  // never
let z5: (E.A | E.B | E.C) & (E.B | E.C | E.D) & (E.C | E.D | E.E) & (E.D | E.E | E.F);  // never