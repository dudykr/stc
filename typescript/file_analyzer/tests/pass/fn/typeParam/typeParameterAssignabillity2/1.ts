function foo3<T extends U, U extends V, V>(t: T, u: U, v: V) {
    u = t;

    v = t; // ok

    v = u; // ok
}