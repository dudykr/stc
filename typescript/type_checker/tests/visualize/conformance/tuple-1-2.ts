var temp = ["s", "t", "r"];
var temp1 = [1, 2, 3];
interface myArray extends Array<Number> { }

var c2: myArray = [...temp1, ...temp];