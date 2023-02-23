// @strict: true
// @declaration: true


declare let ff1: (...rest: [string, string] | [string, number]) => void;
declare let ff2: (x: string, ...rest: [string] | [number]) => void;

ff1 = ff2;
ff2 = ff1;


export { }