interface I13 { x: string };
type T13 = I13;
var x13_1: I13;
var x13_2: T13

x13_1.x
x13_2.x

declare function foo13<T1 extends I13, T2 extends T13>(t1: T1, t2: T13): void;
foo13(x13_1, x13_2);
foo13(x13_2, x13_1);

type T14 = string;
var x14: T14;

declare function foo14_1(x: T14): void;

declare function foo14_2(x: "click"): void;
declare function foo14_2(x: T14): void;

type Meters = number

enum E { x = 10 }

declare function f15(a: string): boolean;
declare function f15(a: Meters): string;
f15(E.x).toLowerCase();

type StringAndBoolean = [string, boolean]
declare function f16(s: StringAndBoolean): string;
var x: [string, boolean];
f16(x);

var y: StringAndBoolean = ["1", false];
y[0].toLowerCase();

export { }