interface I {
    [x: string]: Date;
}

interface I2 extends I {
}

var i: I;
var r: string = i[1]; // error: numeric indexer returns the type of the string indexer

var i2: I2;
var r2: string = i2[1]; // error: numeric indexer returns the type of the string indexer