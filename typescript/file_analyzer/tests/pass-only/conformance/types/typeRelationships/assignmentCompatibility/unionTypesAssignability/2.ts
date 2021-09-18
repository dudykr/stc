// type parameters
function foo<T, U>(t: T, u: U) {
    var x: T | U;
    x = u; // ok
}


export { }