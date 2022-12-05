//@strict: false

interface ABI {
  kind: "a" | "b";
}

declare class CA {
  kind: "a";
  a: string;
  x: number;
}
declare class CB {
  kind: "b";
  b: string;
  y: number;
}

function bar<T extends CA | CB>(x: T & CA) {
  let ab: ABI = x;
}
