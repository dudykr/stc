// @target: es5
var s: string;
var n: number;
var a: any;
var v = {
    get [s + s]() { return 0; },
}