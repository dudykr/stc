// call signatures in derived types must have the same or fewer optional parameters as the base type

interface Base {
    a: new () => number;
    a2: new (x?: number) => number;
    a3: new (x: number) => number;
    a4: new (x: number, y?: number) => number;
    a5: new (x?: number, y?: number) => number;
    a6: new (x: number, y: number) => number;
}
declare var b: Base;

declare var a: new () => number;
a = b.a4; // error

export { }