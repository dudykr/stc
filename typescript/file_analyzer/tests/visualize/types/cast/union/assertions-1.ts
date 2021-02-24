export interface I1 {
    p1: number
}

export interface I2 extends I1 {
    p2: number;
}

export var x = { p1: 10, p2: 20 };
export var y: number | I2 = x;
export var z: I1 = x;

x;
y;
z;

export var a = <number | I2>z;
export var c = <I2>z;
export var d = <I1>y;

a;
c;
d;