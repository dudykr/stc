// Function typed arguments with multiple signatures must be passed an implementation that matches all of them
// Inferences are made quadratic-pairwise to and from these overload sets

module GenericParameter {
    function foo5<T>(cb: { (x: T): string; (x: number): T }) {
        return cb;
    }

    var r6 = foo5(<T>(x: T) => x); // ok

}