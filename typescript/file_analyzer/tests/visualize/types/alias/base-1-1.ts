// Writing a reference to a type alias has exactly the same effect as writing the aliased type itself.

type T1 = number;
var x1: number;
var x1: T1;

type T2 = string;
var x2: string;
var x2: T2;

type T3 = boolean;
var x3: boolean;
var x3: T3;

type T4 = void;
var x4: void;
var x4: T4;

type T5 = any;
var x5: any;
var x5: T5;

interface I6 { x: string }
type T6 = I6;
var x6: I6;
var x6: T6;

x6.x
x6.x

export { }