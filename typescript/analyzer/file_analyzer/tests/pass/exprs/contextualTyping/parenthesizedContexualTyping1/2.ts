
function fun<T>(g: (x: T) => T, x: T): T;
function fun<T>(g: (x: T) => T, h: (y: T) => T, x: T): T;
function fun<T>(g: (x: T) => T, x: T): T {
    return g(x);
}

var f = fun((x => x), (x => x), 10);

export { }