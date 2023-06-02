// call signatures in derived types must have the same or fewer optional parameters as the base type

interface Base {
    a: (...args: number[]) => number;
    a2: (x: number, ...z: number[]) => number;
    a3: (x: number, y?: string, ...z: number[]) => number;
    a4: (x?: number, y?: string, ...z: number[]) => number;
}



























export interface I10E extends Base {
    a3: (x: number, ...z: string[]) => number;  // error
}








