
var b = a + foo()
a;
b
for (var a of [1, 2, 3, foo()]) {
    a;
    b;
}

var foo = function () {
    console.log(a);
    return 5
}

export { }