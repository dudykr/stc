
declare function f2<T, Key extends keyof T>(obj: { [K in keyof T]: T[K] }, key: Key): T[Key];

declare const obj: { a: string } & { b: string };
f2(obj, 'a');
f2(obj, 'b');


export { }