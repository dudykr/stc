declare function f(n: number): void;
declare function f(cb: () => (n: number) => number): void;

f(() => n => n);
