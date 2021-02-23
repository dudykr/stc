class C {
    1: number;
    1.1: string;
}

var c: C;
export var r1 = c[1];
export var r2 = c[1.1];
export var r3 = c['1'];
export var r4 = c['1.1'];