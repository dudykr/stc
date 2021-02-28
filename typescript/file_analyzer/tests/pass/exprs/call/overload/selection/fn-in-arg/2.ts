
function fun<T>(g: (x: T) => T, x: T): T;
function fun<T>(g: (x: T) => T, h: (y: T) => T, x: T): T;
function fun<T>(g: (x: T) => T, x: T): T {
    return g(x);
}

export var k = fun((Math.random() < 0.5 ? (x => x) : (x => undefined)), x => x, 10);