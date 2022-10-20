interface A {
    [n: number]: string;
    [s: string]: number;
}

// All of these should fail.
interface B extends A {
    c: string;
    3: string;
    Infinity: string;
    "-Infinity": string;
    NaN: string;
    "-NaN": string;
    6(): string;
}
