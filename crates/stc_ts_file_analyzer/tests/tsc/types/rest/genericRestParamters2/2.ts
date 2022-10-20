declare const t2: [string, ...boolean[]];

declare const f20: <T extends unknown[]>(...args: T) => T;

f20(42, ...t2);

export { }