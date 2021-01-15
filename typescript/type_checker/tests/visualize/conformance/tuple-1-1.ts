
var temp2: [number[], string[]] = [[1, 2, 3], ["hello", "string"]];

interface tup {
    0: number[] | string[];
    1: number[] | string[];
}
var c0: tup = [...temp2];                         // Error