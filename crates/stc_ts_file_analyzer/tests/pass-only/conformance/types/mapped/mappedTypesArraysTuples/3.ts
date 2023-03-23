// @strict: true
// @declaration: true

declare function nonpartial<T>(x: Partial<T>): T;


declare let x21: (number | undefined)[];
var y21 = nonpartial(x21);
var y21: number[]

export { y21 }