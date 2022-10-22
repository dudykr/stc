// When a function expression with no type parameters and no parameter type annotations 
// is contextually typed (section 4.19) by a type T and a contextual signature S can be extracted from T

// A contextual signature S is extracted from a function type T as follows:
//      Otherwise, no contextual signature can be extracted from T and S is undefined.
var b6: ((s: string, w: boolean) => void) | ((n: number) => number);
b6 = (k) => { k.toLowerCase() };
b6 = (i) => {
    i.toExponential();
    return i;
};                   // Per spec, no contextual signature can be extracted in this case. (Otherwise clause)

export { }