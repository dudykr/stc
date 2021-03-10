
export function f2<T>(x: Partial<T>, y: Readonly<T>) {
    let obj: { [x: string]: any };
    obj = x;
    obj = y;
}
