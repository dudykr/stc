// @strict: true
// @declaration: true

declare function nonpartial<T>(x: Partial<T>): T;


declare let x21: (number | undefined)[];
export let y21 = nonpartial(x21);
