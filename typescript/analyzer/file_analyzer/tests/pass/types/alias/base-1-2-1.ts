class C7 { x: boolean }
type T7 = C7;
var x7: C7;
var x7: T7;

type T8 = string | boolean;
var x8: string | boolean;
var x8: T8;

type T9 = () => string;
var x9: () => string;
var x9: T9;

type T10 = { x: number };
var x10: { x: number };
var x10: T10;

type T11 = { new(): boolean };
var x11: { new(): boolean };
var x11: T11;

interface I13 { x: string };
type T13 = I13;
var x13_1: I13;
var x13_2: T13

x13_1.x
x13_2.x

export { }