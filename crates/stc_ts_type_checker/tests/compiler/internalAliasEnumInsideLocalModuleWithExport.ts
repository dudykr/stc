//@module: commonjs
// @declaration: true
export module a {
    export enum weekend {
        Friday,
        Saturday,
        Sunday
    }
}

export module c {
    export import b = a.weekend;
    export var bVal: b = b.Sunday;
}
