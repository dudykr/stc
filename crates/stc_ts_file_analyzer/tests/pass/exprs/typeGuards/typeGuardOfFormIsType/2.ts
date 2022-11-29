
class C1 {
    p1: string;
}
class C2 {
    p2: number;
}
class D1 extends C1 {
    p3: number;
}

function isC1(x: any): x is C1 {
    return true;
}



var c2Ord1: C2 | D1;
export var r2: false | D1 = isC1(c2Ord1) && c2Ord1; // C2 | D1

