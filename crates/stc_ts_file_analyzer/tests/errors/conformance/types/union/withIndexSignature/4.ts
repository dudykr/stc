//@strict: true

const sym = Symbol();
type Both =
  | { s: number; "0": number; [sym]: boolean }
  | { [n: number]: number; [s: string]: string | number };
declare var both: Both;

both[sym] = "not ok";
