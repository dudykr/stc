interface StrNum extends Array<string | number> {
  0: string;
  1: number;
  length: 2;
}

var x: [string, number];
var y: StrNum;
var z: {
  0: string;
  1: number;
  length: 2;
};

x = y;
x = z; // should get ts2322
y = x; // should pass
y = z;
z = x; // should pass
z = y;
