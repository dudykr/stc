interface I13 { x: string };
type T13 = I13;
var x13_1: I13;
var x13_2: T13

x13_1.x
x13_2.x

declare function foo13<T1 extends I13, T2 extends T13>(t1: T1, t2: T13): void;
foo13(x13_1, x13_2);
foo13(x13_2, x13_1);

export { }