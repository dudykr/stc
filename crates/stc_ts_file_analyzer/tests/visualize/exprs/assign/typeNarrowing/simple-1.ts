export let x: string | number;
x = 1000;
do {
    x; // number
    x = "";
} while (x = x.length)
x; // number