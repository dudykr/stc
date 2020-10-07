class ProtectedGeneric<T> {
    private privateMethod() {
    }

    protected protectedMethod() {
    }
}

class ProtectedGeneric2<T> {
    private privateMethod() {
    }

    protected protectedMethod() {
    }
}

declare let x: ProtectedGeneric<{ a: void; }> & ProtectedGeneric<{ a: void; b: void; }>;
let p1 = x.privateMethod(); // Error, private constituent makes method inaccessible
let p2 = x.protectedMethod(); // Error, protected when all constituents are protected