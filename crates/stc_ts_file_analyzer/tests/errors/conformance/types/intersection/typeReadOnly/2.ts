interface Base {
    readonly value: number;
}
interface Identical {
    readonly value: number;
}

let identical: Base & Identical;
identical.value = 12; // error, lhs can't be a readonly property