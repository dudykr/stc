interface Base {
    readonly value: number;
}

let base: Base;
base.value = 12 // error, lhs can't be a readonly property

