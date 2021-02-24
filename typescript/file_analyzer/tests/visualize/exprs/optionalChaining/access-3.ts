
declare const o3: { b: undefined | { c: string } };
o3["b"]?.c;
o3.b?.["c"];

export { }