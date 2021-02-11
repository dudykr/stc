export class C1 {
    p1: string;
}
export class C2 {
    p2: number;
}
export class D1 extends C1 {
    p3: number;
}

export var ctor2: C2 | D1;
export var r2: D1 | C2 = ctor2 instanceof C1 && ctor2; // C2 | D1
