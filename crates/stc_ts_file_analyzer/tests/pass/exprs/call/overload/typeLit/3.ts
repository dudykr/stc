// Function typed arguments with multiple signatures must be passed an implementation that matches all of them
// Inferences are made quadratic-pairwise to and from these overload sets

var a: {
    (x: boolean): boolean;
    (x: string): string;
}

function foo4(cb: typeof a) {
    return cb;
}


var r4 = foo4(x => x);
r4

export { }