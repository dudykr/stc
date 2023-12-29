// @strictNullChecks: true
// @declaration: true

enum E { A, B, C };

type K15 = keyof E;  // "toString" | "toFixed" | "toExponential" | ...
