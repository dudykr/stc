//@strict: true

declare function f22<T extends unknown[] = []>(args: [...T, number]): T;
declare function f22<T extends unknown[] = []>(args: [...T]): T;

function f23<U extends string[]>(args: [...U, number]) {
    let v2 = f22(["foo", "bar"]);  // [string, string]
}

export { }