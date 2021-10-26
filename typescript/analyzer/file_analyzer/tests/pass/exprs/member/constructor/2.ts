// valid uses of arrays of function types

var x: new () => string[];

var x2: { new(): string }[];
var r3 = x[1];
var r4 = new r3();
var r4b = new r3();

export { }