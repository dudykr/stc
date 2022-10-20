declare const t3: [...boolean[]];

declare const f20: <T extends unknown[]>(...args: T) => T;

f20(42, "hello", ...t3);


export { }