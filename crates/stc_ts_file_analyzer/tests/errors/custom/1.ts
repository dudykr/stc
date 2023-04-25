// @strictNullChecks: true
// @declaration: true

interface Shape {
  name: string;
  width: number;
  height: number;
  visible: boolean;
}

interface Named {
  name: string;
}

interface Point {
  x: number;
  y: number;
}

// Constraint checking

type T02 = { [P in Date]: number }; // Error
type T03 = Record<Date, number>; // Error
