export function f10(x: unknown) {
    x !== 10;
    x >= 0;  // Error
    x.foo;  // Error
    x[10];  // Error
    x();  // Error
}
