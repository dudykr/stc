class Narrow {
    narrowed: boolean
}

var a: object

if (a instanceof Narrow) {
    a.narrowed; // ok
}