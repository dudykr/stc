declare function f2<a>(as1: a[], as2: a[], cmp: (a1: a, a2: a) => number): void;
f2(Array.from([0]), [], (a1, a2) => a1 - a2);

export { }