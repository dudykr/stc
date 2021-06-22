interface A {
    a: string;
}

interface B {
    b: string;
}

// a type guard for B
function isB(toTest: any): toTest is B {
    return toTest && toTest.b;
}

// a function that turns an A into an A & B
function union(a: A): A & B | null {
    if (isB(a)) {
        return a;
    } else {
        return null;
    }
}