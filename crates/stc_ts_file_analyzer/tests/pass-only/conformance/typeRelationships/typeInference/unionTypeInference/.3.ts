// @strict: true
// @target: esnext


declare function bar<T>(x: T, y: string | T): T;
export const y = bar(1, 2);

