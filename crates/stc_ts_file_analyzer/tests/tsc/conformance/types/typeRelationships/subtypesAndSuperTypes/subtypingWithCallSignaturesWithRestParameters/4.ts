// call signatures in derived types must have the same or fewer optional parameters as the base type

interface Base {
    a: (...args: number[]) => number;
    a2: (x: number, ...z: number[]) => number;
    a3: (x: number, y?: string, ...z: number[]) => number;
    a4: (x?: number, y?: string, ...z: number[]) => number;
}

































export interface I16 extends Base {
    a4: (x: number, ...args: string[]) => number;  // error, rest param has type mismatch
}


