export function f1<T>(x: Partial<T>, y: Readonly<T>) {
    let obj: {};
    obj = x;
    obj = y;
}
