// @target: es5
var s: string;
var n: number;
var a: any;
var v = {
    get [<any>true]() { return 0; },
}