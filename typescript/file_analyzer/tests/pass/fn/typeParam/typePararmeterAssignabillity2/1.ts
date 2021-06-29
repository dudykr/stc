function foo3<T extends U, U extends V, V>(t: T, u: U, v: V) {
    t = u; // error
    u = t;

    t = v; // error
    v = t; // ok

    u = v; // error
    v = u; // ok
}