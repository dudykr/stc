// @strict

const enum E { A, B, C, D, E, F }

let x3: ('a' | 'b' | 'c') & ('d' | 'e' | 'f');  // never
let y3: (0 | 1 | 2) & (3 | 4 | 5);  // never
let z3: (E.A | E.B | E.C) & (E.D | E.E | E.F);  // never