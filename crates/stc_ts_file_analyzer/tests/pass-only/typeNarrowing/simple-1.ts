export let x: string | number;
x = 1000;
do {
    x; // number
    x = "";
} while (x = x.length)
declare function assertNumber(n: number): void;
assertNumber(x)