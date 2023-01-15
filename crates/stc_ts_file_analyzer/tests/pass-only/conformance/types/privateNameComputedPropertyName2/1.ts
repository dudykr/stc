let getX: (a: A) => number;

class A {
    [((a: A) => null, "_")]() { }
}
