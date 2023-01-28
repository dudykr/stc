//@strict: true

declare function f22<T extends unknown[] = []>(args: [...T, number]): T;
declare function f22<T extends unknown[] = []>(args: [...T]): T;

function f23<U extends string[]>(args: [...U, number]) {
    let v1 = f22(args);  // U
    let v2 = f22(["foo", "bar"]);  // [string, string]
    let v3 = f22(["foo", 42]);  // [string]
}

export { }