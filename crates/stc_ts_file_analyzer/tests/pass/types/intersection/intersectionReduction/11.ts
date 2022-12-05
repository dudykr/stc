//@strict: false

interface A2 {
  kind: "A";
  a: number;
}

interface B2 {
  kind: "B";
  b: number;
}

function inGeneric<T extends A2 | B2>(alsoShouldBeB: T & B2) {
  const b: B2 = alsoShouldBeB;
}
