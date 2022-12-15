//@strict: false

interface A2 {
  kind: "A";
  a: number;
}

interface B2 {
  kind: "B";
  b: number;
}

declare const shouldBeB: (A2 | B2) & B2;
const b: B2 = shouldBeB; // works
