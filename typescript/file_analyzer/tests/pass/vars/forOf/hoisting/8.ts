
var b = a
a;
b
for (var a of [1, 2, 3, foo()]) {
    a;
    b;
}

var foo = function () {
    return 5
}

export { }