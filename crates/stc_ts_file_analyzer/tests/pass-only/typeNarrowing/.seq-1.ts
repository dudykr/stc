let x: string | boolean | number;
let obj: any;

x = true;
(x = "", obj).foo = (x = x.length);

declare function assertNumber(n: number): void;
assertNumber(x)

export { }