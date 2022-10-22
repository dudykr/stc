function func1(x: string | number): number {
    return 0;
}
type Func2 = (x: string) => number;

let func2: Func2 = func1;  // Ok

export { func2 }