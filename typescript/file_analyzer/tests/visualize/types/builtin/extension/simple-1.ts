interface Number {
    doStuff(): string;
    doOtherStuff<T>(x: T): T;
}

var x = 1;
export var a: string = x.doStuff();
export var b: string = x.doOtherStuff('hm');
export var c: string = x['doStuff']();
export var d: string = x['doOtherStuff']('hm');
