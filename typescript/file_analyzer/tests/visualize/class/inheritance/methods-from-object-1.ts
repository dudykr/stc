export class C { foo: string; }

var c: C;
export var r = c.toString();
export var r2 = c.hasOwnProperty('');
export var o: Object = c;
export var o2: {} = c;
