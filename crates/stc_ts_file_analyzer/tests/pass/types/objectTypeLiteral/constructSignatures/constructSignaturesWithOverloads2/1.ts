class C2<T> {
    constructor(x: T, y?: string);
    constructor(x: T, y: string);
    constructor(x: T) { }
}
module C2 {
    export var x = 1;
}

var r2 = new C2(1, '');
