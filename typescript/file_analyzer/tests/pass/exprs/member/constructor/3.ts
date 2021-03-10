// valid uses of arrays of function types

var x2: { new(): string }[];

var x3: Array<new () => string>;
var r5 = x2[1];
var r6 = new r5();

export { }