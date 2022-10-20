// checking assignment compatibility relations for function types. All of these are valid.

var a15: {
    new(x: number): number[];
    new(x: string): string[];
}
var b15: new <T>(x: T) => T[];
b15 = a15; // ok

export { }