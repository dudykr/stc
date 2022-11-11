export function f10(x: unknown) {
    x == 5;
    x !== 10;
    x >= 0;  // Error
    x.foo;  // Error
    x[10];  // Error
    x();  // Error
    x + 1;  // Error
    x * 2;  // Error
    -x;  // Error
    +x;  // Error
}
