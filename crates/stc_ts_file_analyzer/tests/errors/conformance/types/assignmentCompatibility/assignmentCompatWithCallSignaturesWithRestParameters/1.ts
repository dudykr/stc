// call signatures in derived types must have the same or fewer optional parameters as the target for assignment

interface Base {
    a: (...args: number[]) => number;
    a2: (x: number, ...z: number[]) => number;
    a3: (x: number, y?: string, ...z: number[]) => number;
    a4: (x?: number, y?: string, ...z: number[]) => number;
}

declare var a: (...args: number[]) => number; // ok, same number of required params
a = (x?: string) => 1; // error, incompatible type

export { }