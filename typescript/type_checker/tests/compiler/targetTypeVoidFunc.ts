function f1(): { new (): number; } {
    return function () { return; }
}; 

var x = f1();
var y = new x();
var z = new (f1())();