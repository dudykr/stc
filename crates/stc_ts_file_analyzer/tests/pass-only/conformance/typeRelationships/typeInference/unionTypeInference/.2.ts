// @strict: true
// @target: esnext

declare function f1<T>(x: T, y: string | T): T;

export const a6 = f1(true, false);  // boolean
