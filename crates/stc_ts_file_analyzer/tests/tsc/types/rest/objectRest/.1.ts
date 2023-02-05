// @target: es2015
var o = { a: 1, b: 'no' }


let computed = 'b';
let computed2 = 'a';
var { [computed]: stillNotGreat, [computed2]: soSo, ...o } = o;
({ [computed]: stillNotGreat, [computed2]: soSo, ...o } = o);

export { }