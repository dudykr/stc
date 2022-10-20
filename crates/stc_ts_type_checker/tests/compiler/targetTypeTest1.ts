declare class Point
{
      constructor(x: number, y: number);
      public x: number;
      public y: number;
      public add(dx: number, dy: number): Point;
      static origin: Point;

}

// Type provided by extern declaration
// Because Point is a constructor function, this is inferred
// to be Point and return type is inferred to be void
function Point(x, y) {
    this.x = x;
    this.y = y;
}

declare function EF1(a:number, b:number):number;

function EF1(a,b) { return a+b; }

var x = EF1(1,2);

// Point.origin declared as type Point
Point.origin = new Point(0, 0);

// Point.prototype declared as type Point
// this inferred as Point because of obj.prop assignment
// dx, dy, and return type inferred using target typing
Point.prototype.add = function(dx, dy) {
    return new Point(this.x + dx, this.y + dy);
};

var f : number = 5;

// Object literal type inferred using target typing
// this in function add inferred to be type of object literal (i.e. Point)
// dx, dy, and return type of add inferred using target typing
Point.prototype = {
    x: 0,
    y: 0,
    add: function(dx, dy) {
        return new Point(this.x + dx, this.y + dy);
    }
};

declare var z;
z = function(a: number) {
    a
}

declare class C {
    constructor(a:number, b:number);
	public a : number;
	public b: number;    
	C1M1(c:number,d:number):number;
} 

function C(a,b) {
	this.a=a;
	this.b=b;
}

C.prototype = 
	{	a:0,
		b:0, 
		C1M1: function(c,d) {     
				return (this.a + c) + (this.b + d);
			}
	};
