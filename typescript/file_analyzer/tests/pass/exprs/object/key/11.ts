// @target: es5
var s: string;
var n: number;
var a: any;
var v = {
    get [`hello ${a} bye`]() { return 0; }
}