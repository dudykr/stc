declare const t1: [number, string, ...boolean[]];


declare const f20: <T extends unknown[]>(...args: T) => T;

f20(...t1);


export { }