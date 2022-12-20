interface Base {
    readonly value: number;
}
interface Mutable {
    value: number;
}

let mutable: Base & Mutable;
mutable.value = 12;