interface Base {
    readonly value: number;
}
interface DifferentType {
    readonly value: string;
}

let differentType: Base & DifferentType;
differentType.value = 12; // error, lhs can't be a readonly property
