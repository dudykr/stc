// @strict

const enum E { A, B, C, D, E, F }

let x2: ('a' | 'b' | 'c') & ('c' | 'd' | 'e');  // 'c'
let x4: ('a' | 'b' | 'c') & ('b' | 'c' | 'd') & ('c' | 'd' | 'e');  // 'c'

let y2: (0 | 1 | 2) & (2 | 3 | 4);  // 2
let y4: (0 | 1 | 2) & (1 | 2 | 3) & (2 | 3 | 4);  // 2

let z2: (E.A | E.B | E.C) & (E.C | E.D | E.E);  // E.C
let z4: (E.A | E.B | E.C) & (E.B | E.C | E.D) & (E.C | E.D | E.E);  // E.C
