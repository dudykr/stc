type A = { a: string };
type B = { b: number };
type C = { c: boolean };
type IntesrectionABC<T> = T & A & B & C;
type ABC = { a: string; d: number };
function some<T>() {
    let foo: ABC = {} as IntesrectionABC<T>;
}
