// valid uses of arrays of function types

var x: new () => string[];
var r = x[1];
var r2 = new r();
var r2b = r();

export { }