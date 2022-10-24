declare function f<T>(x: { prop: T }): T;

declare const a: { prop: string } & { prop: number };
declare const b: { prop: string & number };

f(a);  // never


export { }