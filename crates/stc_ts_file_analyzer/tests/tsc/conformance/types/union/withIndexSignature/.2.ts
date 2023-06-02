//@strict: true

type Both =
  | { s: number; "0": number; [sym]: boolean }
  | { [n: number]: number; [s: string]: string | number };
declare var both: Both;

both[1] = 0; // not ok
