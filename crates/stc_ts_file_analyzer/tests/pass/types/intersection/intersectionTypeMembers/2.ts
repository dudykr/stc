// An intersection type has those members that are present in any of its constituent types,
// with types that are intersections of the respective members in the constituent types

type F1 = (x: string) => string;
type F2 = (x: number) => number;

var f: F1 & F2;
var s = f("hello");
var n = f(42);

