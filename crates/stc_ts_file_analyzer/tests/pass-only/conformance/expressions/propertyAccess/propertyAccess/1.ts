
enum Compass {
    North, South, East, West
}

var strIndex: { [n: string]: Compass } = { 'N': Compass.North, 'E': Compass.East };

var stringOrNumber: string | number;


var x2 = strIndex[stringOrNumber];
var x2: Compass;

export { x2 }