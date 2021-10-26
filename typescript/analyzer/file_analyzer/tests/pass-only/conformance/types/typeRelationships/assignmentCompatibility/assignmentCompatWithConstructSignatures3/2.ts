// checking assignment compatibility relations for function types. All of these are valid.

var a17: {
    new(x: new (a: number) => number): number[];
    new(x: new (a: string) => string): string[];
};

var b17: new <T>(x: new (a: T) => T) => T[]; // ok
b17 = a17; // ok

export { }