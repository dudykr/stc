export type Circular<T> = { [P in keyof T]: Circular<T> };
type tup = [number, number, number, number];

function foo(arg: Circular<tup>): tup {
    return arg;
}