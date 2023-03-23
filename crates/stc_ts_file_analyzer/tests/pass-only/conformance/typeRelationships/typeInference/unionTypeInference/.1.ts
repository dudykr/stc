// @strict: true
// @target: esnext

declare function f1<T>(x: T, y: string | T): T;

export const a1 = f1(1, 2);  // 1 | 2
