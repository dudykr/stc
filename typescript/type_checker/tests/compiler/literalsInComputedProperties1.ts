// @noImplicitAny: true

let x = {
    1:1,
    [2]:1,
    "3":1,
    ["4"]:1
}
x[1].toExponential();
x[2].toExponential();
x[3].toExponential();
x[4].toExponential();

interface A {
    1:number;
    [2]:number;
    "3":number;
    ["4"]:number;
}

let y:A;
y[1].toExponential();
y[2].toExponential();
y[3].toExponential();
y[4].toExponential();

class C {
    1:number;
    [2]:number;
    "3":number;
    ["4"]:number;
}

let z:C;
z[1].toExponential();
z[2].toExponential();
z[3].toExponential();
z[4].toExponential();

enum X {
    1 = 1,
    [2] = 2,
    "3" = 3,
    ["4"] = 4,
    "foo" = 5,
    ["bar"] = 6
}

let a = X["foo"];
let a0 = X["bar"];

// TODO: make sure that enum still disallow template literals as member names