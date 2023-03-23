type Both =
  | { s: number; "0": number; [sym]: boolean }
  | { [n: number]: number; [s: string]: string | number };
//@strict: true

declare var both: Both;

both[0] = "not ok";
