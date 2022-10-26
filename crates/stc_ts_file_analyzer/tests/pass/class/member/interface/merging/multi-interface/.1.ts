// From mergedInterfacesWithMultipleBases2.ts

// merged interfaces behave as if all extends clauses from each declaration are merged together
// no errors expected

class C<T> {
    a: T;
}

class C2<T> {
    b: T;
}

class C3<T> {
    c: T;
}

class C4<T> {
    d: T;
}

interface A<T> extends C<T>, C3<T> {
    y: T;
}

interface A<T> extends C2<string>, C4<string> {
    z: T;
}

class D implements A<boolean> {
    a: boolean;
    b: string;
    c: boolean;
    d: string;
    y: boolean;
    z: boolean;
}

export { }