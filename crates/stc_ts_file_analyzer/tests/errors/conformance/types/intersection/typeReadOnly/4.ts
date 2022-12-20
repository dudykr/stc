interface Base {
    readonly value: number;
}
interface DifferentName {
    readonly other: number;
}

let differentName: Base & DifferentName;
differentName.value = 12; // error, property 'value' doesn't exist
