// From mergedInterfacesWithMultipleBases2.ts

// merged interfaces behave as if all extends clauses from each declaration are merged together
// no errors expected

class C {
    a: number;
}

class C2 {
    b: number;
}

class C3 {
    c: string;
}

class C4 {
    d: string;
}


interface A extends C, C3 {
    y: string;
}

interface A extends C2, C4 {
    z: string;
}

class D implements A {
    a: number;
    b: number;
    c: string;
    d: string;
    y: string;
    z: string;
}

var a: A;
var r = a.a;


export { }