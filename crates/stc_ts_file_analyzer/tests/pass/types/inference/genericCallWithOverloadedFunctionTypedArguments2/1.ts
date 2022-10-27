// Function typed arguments with multiple signatures must be passed an implementation that matches all of them
// Inferences are made quadratic-pairwise to and from these overload sets

module GenericParameter {

    function foo7<T>(x: T, cb: { (x: T): string; (x: T, y?: T): string }) {
        return cb;
    }

    var r13 = foo7(1, <T>(x: T) => x); // ok

}