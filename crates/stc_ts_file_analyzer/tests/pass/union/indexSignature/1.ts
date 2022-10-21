var numOrDate: number | Date;

// If each type in U has a string index signature, 
// U has a string index signature of a union type of the types of the string index signatures from each type in U.

var unionOfDifferentReturnType: { [a: string]: number; } | { [a: string]: Date; };
numOrDate = unionOfDifferentReturnType[10]; // number | Date

// If each type in U has a numeric index signature, 
// U has a numeric index signature of a union type of the types of the numeric index signatures from each type in U.
var unionOfDifferentReturnType1: { [a: number]: number; } | { [a: number]: Date; };
numOrDate = unionOfDifferentReturnType1["hello"]; // any


export { }