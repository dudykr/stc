var a2: { <T>(x: T): number; (x: string): string; };
var b2: { <T>(x: T): number; (x: string): string; };
var c2: { (x: number): number; <T>(x: T): any; };

var z2 = [a2, b2, c2];
var r6 = z2[0];
export var r7 = r6(''); // any not string
