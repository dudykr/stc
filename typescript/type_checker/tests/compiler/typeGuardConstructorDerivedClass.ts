// Derived class with different structures
class C1 {
    property1: number;
}

class C2 extends C1 {
    property2: number;
}

let var1: C2 | string;
if (var1.constructor === C1) {
    var1; // never
    var1.property1; // error
}
if (var1.constructor === C2) {
    var1; // C2
    var1.property1; // number
}

// Derived classes with the same structure
class C3 {}

class C4 extends C3 {}

let var2: C4 | string;
if (var2.constructor === C3) {
    var2; // never
}
if (var2.constructor === C4) {
    var2; // C4
}

// Disjointly structured classes
class C5 {
    property1: number;
}

class C6 {
    property2: number;
}

let let3: C6 | string;
if (let3.constructor === C5) {
    let3; // never
}
if (let3.constructor === C6) {
    let3; // C6
}

// Classes with the same structure
class C7 {
    property1: number
}

class C8 {
    property1: number;
}

let let4: C8 | string;
if (let4.constructor === C7) {
    let4; // never
}
if (let4.constructor === C8) {
    let4; // C8
}
