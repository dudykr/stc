// When a function expression is inferentially typed (section 4.9.3) and a type assigned to a parameter in that expression references type parameters for which inferences are being made, 
// the corresponding inferred type arguments to become fixed and no further candidate inferences are made for them.

declare function foo<T>(x: T, a: (x: T) => T, b: (x: T) => T);
var r4 = foo(0 as any as unknown, (x) => '', (x) => ''); // any => any
export { }