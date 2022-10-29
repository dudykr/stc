// When a function expression is inferentially typed (section 4.9.3) and a type assigned to a parameter in that expression references type parameters for which inferences are being made, 
// the corresponding inferred type arguments to become fixed and no further candidate inferences are made for them.

function foo<T>(a: (x: T) => T, b: (x: T) => T) {
    var r: (x: T) => T;
    return r;
}

//var r1 = foo((x: number) => 1, (x: string) => ''); // error
var r2 = foo((x: Object) => null, (x: string) => ''); // Object => Object

export { }