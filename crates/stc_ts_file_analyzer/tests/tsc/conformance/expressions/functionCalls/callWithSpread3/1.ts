declare const s_: string[];

declare function fs2_(a: string, b: string, ...c: string[]): void;

fs2_(...s_, ...s_); // error on ...s_
fs2_(...s_, ...s_, ...s_); // error on ...s_

export { }