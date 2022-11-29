
class C1 {
    p1: string;
}
class C2 {
    p2: number;
}
class D1 extends C1 {
    p3: number;
}
var str: string;
var num: number;

function isC1(x: any): x is C1 {
    return true;
}

function isC2(x: any): x is C2 {
    return true;
}

function isD1(x: any): x is D1 {
    return true;
}

export var c1Orc2: C1 | C2;
num = isD1(c1Orc2) && c1Orc2.p3; // D1